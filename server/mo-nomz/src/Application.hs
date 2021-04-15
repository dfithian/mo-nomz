module Application where

import ClassyPrelude

import Control.Monad (fail)
import Control.Monad.Logger (defaultOutput)
import Data.Default (def)
import Data.Pool (createPool)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand(..), MigrationResult(..), runMigrations
  )
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setPort)
import Network.Wai.Middleware.RequestLogger (mkRequestLogger)
import Servant.API ((:<|>)(..))
import Servant.Server (ServerT, hoistServer, serve)
import Servant.Server.StaticFiles (serveDirectoryFileServer)

import Foundation (App(..), NomzServer, runNomzServer, withDbConn)
import Servant (NomzApi, nomzApi, wholeApi)
import Server
  ( deleteIngredient, deleteRecipes, getHealth, getIngredients, getRecipes, postCreateUser
  , postMergeIngredient, postRecipeImportBody, postRecipeImportLink, postUpdateRecipe
  )
import Settings (AppSettings(..), DatabaseSettings(..), staticSettings)

nomzServer :: ServerT NomzApi NomzServer
nomzServer =
  getHealth
    :<|> postCreateUser
    :<|> getIngredients
    :<|> postMergeIngredient
    :<|> deleteIngredient
    :<|> postRecipeImportLink
    :<|> postRecipeImportBody
    :<|> postUpdateRecipe
    :<|> getRecipes
    :<|> deleteRecipes

migrateDatabase :: App -> IO ()
migrateDatabase app = do
  result <- flip runReaderT app $ withDbConn $ \c -> runMigrations True c $
    [ MigrationInitialization
    , MigrationDirectory (appMigrationDir (appSettings app))
    ]
  case result of
    MigrationError str -> fail $ "Failed to run migrations due to " <> str
    MigrationSuccess -> pure ()

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings@AppSettings {..} = do
  let DatabaseSettings {..} = appDatabase
  appConnectionPool <- createPool (connectPostgreSQL $ encodeUtf8 databaseSettingsConnStr) close databaseSettingsPoolsize 15 1
  let appLogFunc = defaultOutput stdout
  pure App {..}

warpSettings :: App -> Settings
warpSettings app =
  setPort (appPort $ appSettings app)
    $ defaultSettings

appMain :: IO ()
appMain = do
  settings <- loadYamlSettingsArgs [staticSettings] useEnv
  app <- makeFoundation settings
  migrateDatabase app
  let appl = serve wholeApi $
        hoistServer nomzApi (runNomzServer app) nomzServer
          :<|> serveDirectoryFileServer (appStaticDir $ appSettings app)
  requestLogger <- mkRequestLogger def
  runSettings (warpSettings app) $ requestLogger appl
