module Settings where

import ClassyPrelude

import Data.Aeson ((.:), (.=), FromJSON, ToJSON, Value, object, parseJSON, toJSON, withObject)

data DatabaseSettings = DatabaseSettings
  { databaseSettingsConnStr  :: Text
  , databaseSettingsPoolsize :: Int
  }

data CacheSettings = CacheSettings
  { cacheSettingsValidSeconds   :: Int
  , cacheSettingsMaxSize        :: Int
  , cacheSettingsRefreshSeconds :: Int
  }

data AppSettings = AppSettings
  { appPort         :: Int -- ^ The port to serve the application on.
  , appDatabase     :: DatabaseSettings -- ^ The database settings.
  , appMigrationDir :: FilePath -- ^ Where the migrations are located.
  , appStaticDir    :: FilePath -- ^ Where the static files are located.
  , appBcryptCost   :: Int -- ^ Bcrypt cost.
  , appCache        :: CacheSettings -- ^ Settings for caching.
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
      <$> obj .: "valid-seconds"
      <*> obj .: "max-size"
      <*> obj .: "refresh-seconds"

instance ToJSON CacheSettings where
  toJSON CacheSettings {..} = object
    [ "valid-seconds" .= cacheSettingsValidSeconds
    , "max-size" .= cacheSettingsMaxSize
    , "refresh-seconds" .= cacheSettingsRefreshSeconds
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

instance ToJSON AppSettings where
  toJSON AppSettings {..} = object
    [ "port" .= appPort
    , "database" .= appDatabase
    , "migration-dir" .= appMigrationDir
    , "static-dir" .= appStaticDir
    , "bcrypt-cost" .= appBcryptCost
    , "cache" .= appCache
    ]

staticSettings :: AppSettings
staticSettings = AppSettings
  { appPort = 8080
  , appDatabase = DatabaseSettings
    { databaseSettingsConnStr = "postgres://postgres:postgres@localhost:5432/postgres"
    , databaseSettingsPoolsize = 3
    }
  , appMigrationDir = "server/mo-nomz/sql/migrations/"
  , appStaticDir = "server/mo-nomz/assets"
  , appBcryptCost = 4
  , appCache = CacheSettings
    { cacheSettingsValidSeconds = 60 * 60 * 24 * 7
    , cacheSettingsMaxSize = 10000
    , cacheSettingsRefreshSeconds = 60 * 60 * 24
    }
  }

staticSettingsValue :: Value
staticSettingsValue = toJSON staticSettings
