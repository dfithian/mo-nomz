module Application where

import NomzPrelude

import Control.Monad.Logger (defaultOutput)
import Data.Tagged (Tagged(..))
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand(..), MigrationResult(..), runMigrations
  )
import Network.HTTP.Types
  ( hContentEncoding, hContentType, hLocation, ok200, temporaryRedirect307, unauthorized401
  )
import Network.Wai (pathInfo, responseFile, responseLBS)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setPort)
import Network.Wai.Middleware.RequestLogger (OutputFormat(Detailed), mkRequestLogger, outputFormat)
import Servant.API ((:<|>)(..))
import Servant.Server (ServerT, hoistServer, serve)
import Servant.Server.StaticFiles (serveDirectoryWith)
import System.IO (stdout)
import WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import WaiAppStatic.Types (ss404Handler, ssIndices, ssListing, unsafeToPiece)
import qualified Data.Text.Encoding as Text
import qualified Network.Wai.Middleware.EnforceHTTPS as EnforceHTTPS

import Foundation (App(..), LogFunc, NomzServer, createManager, runNomzServer, withDbConn)
import Scraper.Site (isInvalidScraper)
import Servant (NomzApi, nomzApi, wholeApi)
import Server (getHealth, postCreateUser, postParseBlob, postParseLink, postPingUser)
import Settings (AppSettings(..), DatabaseSettings(..), staticSettingsValue)
import qualified Database

nomzServer :: ServerT NomzApi NomzServer
nomzServer =
  getHealth
    :<|> postCreateUser
    :<|> postPingUser
    :<|> postParseBlob
    :<|> postParseLink

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
  appConnectionPool <- createPool (connectPostgreSQL $ Text.encodeUtf8 databaseSettingsConnStr) close databaseSettingsPoolsize 15 1
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
  let staticDir = appStaticDir $ appSettings app
      staticFileSettings = (defaultFileServerSettings staticDir)
        { ss404Handler = Just $ \req respond ->
            respond $ case headMay (pathInfo req) == Just "api" of
              True -> responseLBS unauthorized401 [] "Unauthorized"
              False -> responseLBS temporaryRedirect307 [(hLocation, "/")] ""
        , ssListing = Nothing
        , ssIndices = fmap unsafeToPiece ["index.html", "index.htm"]
        }
      ssl = case appForceSsl settings of
        True -> EnforceHTTPS.withResolver EnforceHTTPS.xForwardedProto
        False -> id
      appl = serve wholeApi $
        hoistServer nomzApi (runNomzServer app) nomzServer
          :<|> Tagged ( \_req respond ->
            respond $
              responseFile
                ok200
                [(hContentType, "application/json; charset=utf8"), (hContentEncoding, "gzip")]
                (staticDir <> "/apple-app-site-association")
                Nothing
            )
          :<|> serveDirectoryWith staticFileSettings
  requestLogger <- mkRequestLogger def { outputFormat = Detailed False }
  runSettings (warpSettings app) $ requestLogger $ ssl appl
