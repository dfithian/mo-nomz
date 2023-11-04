module Application where

import Prelude

import Chez.Grater.Manager (createManager)
import Data.Aeson ((.=), object)
import Data.Default (def)
import Data.Tagged (Tagged(..))
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Network.HTTP.Types
  ( hContentEncoding, hContentType, hLocation, notFound404, ok200, temporaryRedirect307
  )
import Network.Wai (pathInfo, responseFile, responseLBS)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setPort)
import Network.Wai.Middleware.RequestLogger (OutputFormat(Detailed), mkRequestLogger, outputFormat)
import Servant.API ((:<|>)(..))
import Servant.Server (ServerT, hoistServer, serve)
import Servant.Server.StaticFiles (serveDirectoryWith)
import WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import WaiAppStatic.Types (ss404Handler, ssIndices, ssListing, unsafeToPiece)
import qualified Network.Wai.Middleware.EnforceHTTPS as EnforceHTTPS

import Foundation (App(..), NomzServer, runNomzServer)
import Servant (NomzApi, nomzApi, wholeApi)
import Server (postParseBlob, postParseLink)
import Settings (AppSettings(..), staticSettingsValue)

nomzServer :: ServerT NomzApi NomzServer
nomzServer =
  (pure $ object ["userId" .= (1 :: Int), "apiToken" .= ("ignored" :: String)])
    :<|> (\_ -> pure $ object ["status" .= ("pong" :: String)])
    :<|> postParseBlob
    :<|> postParseLink
    :<|> (\_ -> postParseBlob)
    :<|> (\_ -> postParseLink)

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
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
  let staticDir = appStaticDir $ appSettings app
      staticFileSettings = (defaultFileServerSettings staticDir)
        { ss404Handler = Just $ \req respond ->
            respond $ case pathInfo req of
              "api" : _ -> responseLBS notFound404 [] "Not Found"
              _ -> responseLBS temporaryRedirect307 [(hLocation, "/")] ""
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
