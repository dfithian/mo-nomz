module Application where

import ClassyPrelude

import Control.Monad.Logger (defaultOutput)
import Data.Default (def)
import Data.Pool (createPool)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Database.PostgreSQL.Config (pgConnStr, pgPoolSize, pgPoolStripes)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setPort)
import Network.Wai.Middleware.RequestLogger (mkRequestLogger)
import Servant.API ((:<|>)(..))
import Servant.Server (ServerT, hoistServer, serve)

import Foundation (App(..), NomzServer, runNomzServer)
import Servant (NomzApi, nomzApi)
import Server
  ( deleteIngredient, getIngredients, postCreateUser, postMergeIngredient, postRecipeImportLink
  )
import Settings (AppSettings(..))

nomzServer :: ServerT NomzApi NomzServer
nomzServer =
  postCreateUser
    :<|> postRecipeImportLink
    :<|> getIngredients
    :<|> postMergeIngredient
    :<|> deleteIngredient

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings@AppSettings {..} = do
  appConnectionPool <- createPool (connectPostgreSQL $ pgConnStr appPostgresConf) close (pgPoolSize appPostgresConf) 15 (pgPoolStripes appPostgresConf)
  let appLogFunc = defaultOutput stdout
  pure App {..}

warpSettings :: App -> Settings
warpSettings app =
  setPort (appPort $ appSettings app)
    $ defaultSettings

appMain :: IO ()
appMain = do
  settings <- loadYamlSettings ["config/settings.yml"] [] useEnv
  app <- makeFoundation settings
  let appl = serve nomzApi $ hoistServer nomzApi (runNomzServer app) nomzServer
  requestLogger <- mkRequestLogger def
  runSettings (warpSettings app) $ requestLogger appl
