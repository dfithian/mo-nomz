module Application where

import Prelude

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput, runLoggingT)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Default (def)
import Data.Pool (Pool, createPool)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Data.Version (showVersion)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand(..), MigrationResult(..), runMigrations
  )
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setPort)
import Network.Wai.Middleware.RequestLogger (OutputFormat(Detailed), mkRequestLogger, outputFormat)
import Servant.API ((:<|>)(..))
import Servant.Server (ServerT, hoistServer, serve)
import Servant.Server.StaticFiles (serveDirectoryWith)
import System.IO (stdout)
import Text.Blaze ((!), Markup)
import WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import WaiAppStatic.Types (ssListing)
import qualified Data.Text as Text
import qualified Network.Wai.Middleware.EnforceHTTPS as EnforceHTTPS
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlAttr

import Foundation (App(..), AppM, LogFunc, NomzServer, createManager, runNomzServer, withDbConn)
import Paths_mo_nomz (version)
import Scrape (isInvalidScraper)
import Servant (NomzApi, nomzApi, wholeApi)
import Server
  ( deleteGroceryItem, deleteRecipes, getExport, getGroceryItems, getHealth, getRecentUsers
  , getRecipe, getRecipes, getRecipesV1, postClearGroceryItems, postCreateUser
  , postGroceryImportBlob, postMergeGroceryItem, postParseBlob, postParseLink, postRecipeImportLink
  , postUpdateGroceryItem, postUpdateRecipe, postUpdateRecipeIngredients
  )
import Settings (AppSettings(..), DatabaseSettings(..), staticSettingsValue)
import Types (tshow)
import qualified Database

getMetrics :: AppM m => m Markup
getMetrics = do
  let renderMetric (key, value) = Html.div (Html.span (Html.toHtml (Text.unwords [key, tshow value])))
  App {..} <- ask
  now <- liftIO getCurrentTime
  (dayUsers, weekUsers, monthUsers, yearUsers) <- getRecentUsers
  healthHtml <- Html.div (Html.span (Html.text "Health OK")) <$ getHealth
  let uptimeHtml = Html.div (Html.span (Html.text $ "Started at " <> pack (formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") appStarted <> " UTC")))
      refreshHtml = Html.div (Html.span (Html.text $ "Last refreshed at " <> pack (formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") now <> " UTC")))
      versionHtml = Html.div (Html.span (Html.text $ "Version " <> pack (showVersion version)))
      metricsHtml = mconcat . fmap renderMetric $
        [ ("recent_users_day", dayUsers)
        , ("recent_users_week", weekUsers)
        , ("recent_users_month", monthUsers)
        , ("recent_users_year", yearUsers)
        ]
  pure $ Html.html $ do
    Html.head $ do
      Html.meta ! HtmlAttr.httpEquiv "Refresh" ! HtmlAttr.content "300"
      Html.style $ Html.text "span { font-family: Courier New; font-size: 14px; }"
    Html.body $ mconcat [uptimeHtml, refreshHtml, versionHtml, healthHtml, metricsHtml]

nomzServer :: ServerT NomzApi NomzServer
nomzServer =
  getMetrics
    :<|> getHealth
    :<|> postCreateUser
    :<|> getGroceryItems
    :<|> postUpdateGroceryItem
    :<|> postMergeGroceryItem
    :<|> deleteGroceryItem
    :<|> postClearGroceryItems
    :<|> postGroceryImportBlob
    :<|> postRecipeImportLink
    :<|> postUpdateRecipe
    :<|> postUpdateRecipeIngredients
    :<|> getRecipesV1
    :<|> getRecipes
    :<|> getRecipe
    :<|> deleteRecipes
    :<|> postParseBlob
    :<|> postParseLink
    :<|> getExport

migrateDatabase :: Pool Connection -> LogFunc -> AppSettings -> IO ()
migrateDatabase pool logFunc settings = do
  result <- flip runLoggingT logFunc $ flip runReaderT pool $ withDbConn $ \c -> do
    inner <- runMigrations True c $
      [ MigrationInitialization
      , MigrationDirectory (appMigrationDir settings)
      ]
    Database.invalidateCachedRecipes c isInvalidScraper
    pure inner
  case result of
    Left err -> fail $ "Failed to run migrations due to database exception " <> show err
    Right (MigrationError str) -> fail $ "Failed to run migrations due to " <> str
    Right MigrationSuccess -> pure ()

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings@AppSettings {..} = do
  let DatabaseSettings {..} = appDatabase
      appLogFunc = defaultOutput stdout
  appConnectionPool <- createPool (connectPostgreSQL $ encodeUtf8 databaseSettingsConnStr) close databaseSettingsPoolsize 15 1
  migrateDatabase appConnectionPool appLogFunc appSettings
  appManager <- createManager
  appStarted <- getCurrentTime
  pure App {..}

warpSettings :: App -> Settings
warpSettings app =
  setPort (appPort $ appSettings app)
    $ defaultSettings

appMain :: IO ()
appMain = do
  settings <- loadYamlSettingsArgs [staticSettingsValue] useEnv
  app <- makeFoundation settings
  let staticFileSettings = (defaultFileServerSettings $ appStaticDir $ appSettings app)
        { ssListing = Nothing
        }
      ssl = case appForceSsl settings of
        True -> EnforceHTTPS.withResolver EnforceHTTPS.xForwardedProto
        False -> id
      appl = serve wholeApi $
        hoistServer nomzApi (runNomzServer app) nomzServer
          :<|> serveDirectoryWith staticFileSettings
  requestLogger <- mkRequestLogger def { outputFormat = Detailed False }
  runSettings (warpSettings app) $ requestLogger $ ssl appl
