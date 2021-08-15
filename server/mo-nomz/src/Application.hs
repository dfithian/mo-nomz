module Application where

import ClassyPrelude

import Control.Monad (fail)
import Control.Monad.Logger (defaultOutput, runLoggingT)
import Data.Default (def)
import Data.Pool (createPool)
import Data.Time.Clock (diffUTCTime)
import Data.Time.Format (iso8601DateFormat)
import Data.Version (showVersion)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand(..), MigrationResult(..), runMigrations
  )
import Network.Wai (Middleware, rawPathInfo)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setPort)
import Network.Wai.Middleware.RequestLogger (mkRequestLogger)
import Servant.API ((:<|>)(..))
import Servant.Server (ServerT, hoistServer, serve)
import Servant.Server.StaticFiles (serveDirectoryWith)
import System.Metrics (Value(..), createCounter, createDistribution, newStore, sampleAll)
import Text.Blaze ((!), Markup)
import WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import WaiAppStatic.Types (ssListing)
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlAttr

import Foundation
  ( App(..), AppMetrics(..), AppM, NomzServer, createManager, runNomzServer, withDbConn
  )
import Paths_mo_nomz (version)
import Servant (NomzApi, nomzApi, wholeApi)
import Server
  ( deleteGroceryItem, deleteRecipes, getGroceryItems, getHealth, getRecipe, getRecipes
  , postClearGroceryItems, postCreateUser, postGroceryImportBlob, postGroceryImportList
  , postMergeGroceryItem, postRecipeImportLink, postUpdateGroceryItem, postUpdateRecipe
  , postUpdateRecipeIngredients
  )
import Settings (AppSettings(..), DatabaseSettings(..), staticSettingsValue)

getMetrics :: AppM m => m Markup
getMetrics = do
  let renderMetric (key, value) =
        let valueStr = case value of
              Counter x -> tshow x
              Gauge x -> tshow x
              Label x -> x
              Distribution x -> tshow $ Distribution.mean x
        in Html.div (Html.span (Html.toHtml (unwords [key, valueStr])))
  App {..} <- ask
  now <- liftIO getCurrentTime
  current <- liftIO $ sampleAll (appMetricsStore appMetrics)
  healthHtml <- Html.div (Html.span (Html.text "Health OK")) <$ getHealth
  let uptimeHtml = Html.div (Html.span (Html.text $ "Started at " <> pack (formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") appStarted <> " UTC")))
      refreshHtml = Html.div (Html.span (Html.text $ "Last refreshed at " <> pack (formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") now <> " UTC")))
      versionHtml = Html.div (Html.span (Html.text $ "Version " <> pack (showVersion version)))
      metricsHtml = mconcat . map renderMetric . sortOn fst . mapToList $ current
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
    :<|> postGroceryImportList
    :<|> postGroceryImportBlob
    :<|> postRecipeImportLink
    :<|> postUpdateRecipe
    :<|> postUpdateRecipeIngredients
    :<|> getRecipes
    :<|> getRecipe
    :<|> deleteRecipes

migrateDatabase :: App -> IO ()
migrateDatabase app = do
  result <- flip runLoggingT (appLogFunc app) $ flip runReaderT app $ withDbConn $ \c -> runMigrations True c $
    [ MigrationInitialization
    , MigrationDirectory (appMigrationDir (appSettings app))
    ]
  case result of
    Left err -> fail $ "Failed to run migrations due to database exception " <> show err
    Right (MigrationError str) -> fail $ "Failed to run migrations due to " <> str
    Right MigrationSuccess -> pure ()

makeAppMetrics :: IO AppMetrics
makeAppMetrics = do
  store <- newStore
  AppMetrics store
    <$> createCounter "total_requests" store
    <*> createDistribution "response_timing" store

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings@AppSettings {..} = do
  let DatabaseSettings {..} = appDatabase
      appLogFunc = defaultOutput stdout
  appConnectionPool <- createPool (connectPostgreSQL $ encodeUtf8 databaseSettingsConnStr) close databaseSettingsPoolsize 15 1
  appManager <- createManager
  appMetrics <- makeAppMetrics
  appStarted <- getCurrentTime
  pure App {..}

warpSettings :: App -> Settings
warpSettings app =
  setPort (appPort $ appSettings app)
    $ defaultSettings

ekgMiddleware :: App -> Middleware
ekgMiddleware App {..} appl req respond = do
  case ("/api" `isPrefixOf` rawPathInfo req) of
    False -> appl req respond
    True -> do
      let AppMetrics {..} = appMetrics
      Counter.inc appMetricsTotalRequests
      start <- getCurrentTime
      received <- appl req respond
      end <- getCurrentTime
      Distribution.add appMetricsResponseTiming (fromInteger $ round $ (* 1000) $ diffUTCTime end start)
      pure received

appMain :: IO ()
appMain = do
  settings <- loadYamlSettingsArgs [staticSettingsValue] useEnv
  app <- makeFoundation settings
  migrateDatabase app
  let staticFileSettings = (defaultFileServerSettings $ appStaticDir $ appSettings app)
        { ssListing = Nothing
        }
      appl = serve wholeApi $
        hoistServer nomzApi (runNomzServer app) nomzServer
          :<|> serveDirectoryWith staticFileSettings
  requestLogger <- mkRequestLogger def
  runSettings (warpSettings app) $ requestLogger $ ekgMiddleware app $ appl
