module Application where

import ClassyPrelude

import Control.Concurrent (forkIO)
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad (fail)
import Control.Monad.Logger (defaultOutput, logError, runLoggingT)
import Data.Default (def)
import Data.Pool (Pool, createPool)
import Data.Time.Clock (diffUTCTime)
import Data.Time.Format (iso8601DateFormat)
import Data.Version (showVersion)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
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
  ( App(..), AppMetrics(..), AppM, LogFunc, NomzServer, createManager, runNomzServer, withDbConn
  )
import Paths_mo_nomz (version)
import Scrape (isInvalidScraper)
import Servant (NomzApi, nomzApi, wholeApi)
import Server
  ( deleteGroceryItem, deleteRecipes, getExport, getGroceryItems, getHealth, getRecentUsers
  , getRecipe, getRecipes, getRecipesV1, postClearGroceryItems, postCreateUser
  , postGroceryImportBlob, postMergeGroceryItem, postParseBlob, postParseLink, postRecipeImportLink
  , postUpdateGroceryItem, postUpdateRecipe, postUpdateRecipeIngredients
  )
import Settings (AppSettings(..), CacheSettings(..), DatabaseSettings(..), staticSettingsValue)
import qualified Database

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
  (dayUsers, weekUsers, monthUsers, yearUsers) <- getRecentUsers
  current <- liftIO $ sampleAll (appMetricsStore appMetrics)
  healthHtml <- Html.div (Html.span (Html.text "Health OK")) <$ getHealth
  let uptimeHtml = Html.div (Html.span (Html.text $ "Started at " <> pack (formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") appStarted <> " UTC")))
      refreshHtml = Html.div (Html.span (Html.text $ "Last refreshed at " <> pack (formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") now <> " UTC")))
      versionHtml = Html.div (Html.span (Html.text $ "Version " <> pack (showVersion version)))
      metricsHtml = mconcat . map renderMetric . sortOn fst $
        mapToList current
          <> [ ("recent_users_day", Gauge dayUsers)
             , ("recent_users_week", Gauge weekUsers)
             , ("recent_users_month", Gauge monthUsers)
             , ("recent_users_year", Gauge yearUsers)
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

makeAppMetrics :: IO AppMetrics
makeAppMetrics = do
  store <- newStore
  AppMetrics store
    <$> createCounter "total_requests" store
    <*> createDistribution "response_timing" store

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings@AppSettings {..} = do
  let DatabaseSettings {..} = appDatabase
      CacheSettings {..} = appCache
      appLogFunc = defaultOutput stdout
  appConnectionPool <- createPool (connectPostgreSQL $ encodeUtf8 databaseSettingsConnStr) close databaseSettingsPoolsize 15 1
  migrateDatabase appConnectionPool appLogFunc appSettings
  appManager <- createManager
  appMetrics <- makeAppMetrics
  appStarted <- getCurrentTime
  appCacheExpire <- forkIO $ flip runLoggingT appLogFunc $ flip runReaderT appConnectionPool $ forever $ do
    liftIO $ delay (1000000 * fromIntegral cacheSettingsRefreshSeconds)
    result <- withDbConn $ \c -> Database.refreshCachedRecipes c cacheSettingsValidSeconds cacheSettingsMaxSize
    case result of
      Left se -> $logError $ "Failed to refresh cashed recipes due to " <> tshow se
      Right () -> pure ()
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
  let staticFileSettings = (defaultFileServerSettings $ appStaticDir $ appSettings app)
        { ssListing = Nothing
        }
      appl = serve wholeApi $
        hoistServer nomzApi (runNomzServer app) nomzServer
          :<|> serveDirectoryWith staticFileSettings
  requestLogger <- mkRequestLogger def
  runSettings (warpSettings app) $ requestLogger $ ekgMiddleware app $ appl
