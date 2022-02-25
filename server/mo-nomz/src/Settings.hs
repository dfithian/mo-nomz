module Settings where

import Prelude

import Data.Aeson ((.:), (.=), FromJSON, ToJSON, Value, object, parseJSON, toJSON, withObject)
import Data.Text (Text)

data DatabaseSettings = DatabaseSettings
  { databaseSettingsConnStr  :: Text
  , databaseSettingsPoolsize :: Int
  }

data CacheSettings = CacheSettings
  { cacheSettingsEnabled      :: Bool
  , cacheSettingsValidSeconds :: Int
  , cacheSettingsMaxSize      :: Int
  }

data AppSettings = AppSettings
  { appPort         :: Int -- ^ The port to serve the application on.
  , appDatabase     :: DatabaseSettings -- ^ The database settings.
  , appMigrationDir :: FilePath -- ^ Where the migrations are located.
  , appStaticDir    :: FilePath -- ^ Where the static files are located.
  , appBcryptCost   :: Int -- ^ Bcrypt cost.
  , appCache        :: CacheSettings -- ^ Settings for caching.
  , appForceSsl     :: Bool -- ^ Whether to force SSL.
  }

instance FromJSON DatabaseSettings where
  parseJSON = withObject "DatabaseSettings" $ \obj ->
    DatabaseSettings
      <$> obj .: "conn-str"
      <*> obj .: "poolsize"

instance ToJSON DatabaseSettings where
  toJSON DatabaseSettings {..} = object
    [ "conn-str" .= databaseSettingsConnStr
    , "poolsize" .= databaseSettingsPoolsize
    ]

instance FromJSON CacheSettings where
  parseJSON = withObject "CacheSettings" $ \obj ->
    CacheSettings
      <$> obj .: "enabled"
      <*> obj .: "valid-seconds"
      <*> obj .: "max-size"

instance ToJSON CacheSettings where
  toJSON CacheSettings {..} = object
    [ "enabled" .= cacheSettingsEnabled
    , "valid-seconds" .= cacheSettingsValidSeconds
    , "max-size" .= cacheSettingsMaxSize
    ]

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \obj ->
    AppSettings
      <$> obj .: "port"
      <*> obj .: "database"
      <*> obj .: "migration-dir"
      <*> obj .: "static-dir"
      <*> obj .: "bcrypt-cost"
      <*> obj .: "cache"
      <*> obj .: "force-ssl"

instance ToJSON AppSettings where
  toJSON AppSettings {..} = object
    [ "port" .= appPort
    , "database" .= appDatabase
    , "migration-dir" .= appMigrationDir
    , "static-dir" .= appStaticDir
    , "bcrypt-cost" .= appBcryptCost
    , "cache" .= appCache
    , "force-ssl" .= appForceSsl
    ]

staticSettings :: AppSettings
staticSettings = AppSettings
  { appPort = 8080
  , appDatabase = DatabaseSettings
    { databaseSettingsConnStr = "postgres://postgres:postgres@localhost:5432/postgres"
    , databaseSettingsPoolsize = 3
    }
  , appMigrationDir = "server/mo-nomz/sql/migrations/"
  , appStaticDir = "client/web/build/"
  , appBcryptCost = 4
  , appCache = CacheSettings
    { cacheSettingsEnabled = False
    , cacheSettingsValidSeconds = 60 * 60 * 24 * 7
    , cacheSettingsMaxSize = 10000
    }
  , appForceSsl = False
  }

testSettings :: AppSettings
testSettings = staticSettings
  { appCache = (appCache staticSettings)
      { cacheSettingsEnabled = True
      }
  }

staticSettingsValue :: Value
staticSettingsValue = toJSON staticSettings
