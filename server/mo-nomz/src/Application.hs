module Application where

import ClassyPrelude

import Control.Monad (fail)
import Control.Monad.Logger (defaultOutput, runLoggingT)
import Data.Default (def)
import Data.Pool (createPool)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand(..), MigrationResult(..), runMigrations
  )
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setPort)
import Network.Wai.Middleware.RequestLogger (mkRequestLogger)
import Servant.API ((:<|>)(..), Header, Headers, addHeader)
import Servant.Server (ServerT, hoistServer, serve)
import Servant.Server.StaticFiles (serveDirectoryWith)
import WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import WaiAppStatic.Types (ssListing)

import Foundation (App(..), AppM, NomzServer, createManager, runNomzServer, withDbConn)
import Servant (NomzApi, nomzApi, wholeApi)
import Server
  ( deleteGroceryItem, deleteRecipes, getGroceryItems, getHealth, getRecipes, postClearGroceryItems
  , postCreateUser, postGroceryImportBlob, postGroceryImportList, postMergeGroceryItem
  , postRecipeImportLink, postUpdateRecipe
  )
import Settings (AppSettings(..), DatabaseSettings(..), staticSettingsValue)

getStaticAsset :: AppM m => m (Headers '[Header "Location" String] ByteString)
getStaticAsset = pure $ addHeader "https://monomzsupport.wordpress.com" ""

nomzServer :: ServerT NomzApi NomzServer
nomzServer =
  getStaticAsset
    :<|> getStaticAsset
    :<|> getHealth
    :<|> postCreateUser
    :<|> getGroceryItems
    :<|> postMergeGroceryItem
    :<|> deleteGroceryItem
    :<|> postClearGroceryItems
    :<|> postGroceryImportList
    :<|> postGroceryImportBlob
    :<|> postRecipeImportLink
    :<|> postUpdateRecipe
    :<|> getRecipes
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

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings@AppSettings {..} = do
  let DatabaseSettings {..} = appDatabase
      appLogFunc = defaultOutput stdout
  appConnectionPool <- createPool (connectPostgreSQL $ encodeUtf8 databaseSettingsConnStr) close databaseSettingsPoolsize 15 1
  appManager <- createManager
  pure App {..}

warpSettings :: App -> Settings
warpSettings app =
  setPort (appPort $ appSettings app)
    $ defaultSettings

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
  runSettings (warpSettings app) $ requestLogger appl
