module Settings where

import Prelude

import Data.Aeson ((.:), (.=), FromJSON, ToJSON, Value, object, parseJSON, toJSON, withObject)

data AppSettings = AppSettings
  { appPort      :: Int -- ^ The port to serve the application on.
  , appStaticDir :: FilePath -- ^ Where the static files are located.
  , appForceSsl  :: Bool -- ^ Whether to force SSL.
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \obj ->
    AppSettings
      <$> obj .: "port"
      <*> obj .: "static-dir"
      <*> obj .: "force-ssl"

instance ToJSON AppSettings where
  toJSON AppSettings {..} = object
    [ "port" .= appPort
    , "static-dir" .= appStaticDir
    , "force-ssl" .= appForceSsl
    ]

staticSettings :: AppSettings
staticSettings = AppSettings
  { appPort = 8080
  , appStaticDir = "client/web/build/"
  , appForceSsl = False
  }

staticSettingsValue :: Value
staticSettingsValue = toJSON staticSettings
