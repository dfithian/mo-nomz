module Settings where

import ClassyPrelude

import Data.Aeson ((.:), (.=), FromJSON, ToJSON, Value, object, parseJSON, toJSON, withObject)

data DatabaseSettings = DatabaseSettings
  { databaseSettingsConnStr  :: Text
  , databaseSettingsPoolsize :: Int
  }

data AppSettings = AppSettings
  { appPort         :: Int -- ^ The port to serve the application on.
  , appDatabase     :: DatabaseSettings -- ^ The database settings.
  , appMigrationDir :: FilePath -- ^ Where the migrations are located.
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

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \obj ->
    AppSettings
      <$> obj .: "port"
      <*> obj .: "database"
      <*> obj .: "migration-dir"

instance ToJSON AppSettings where
  toJSON AppSettings {..} = object
    [ "port" .= appPort
    , "database" .= appDatabase
    , "migration-dir" .= appMigrationDir
    ]

staticSettings :: Value
staticSettings = toJSON $ AppSettings
  { appPort = 8080
  , appDatabase = DatabaseSettings
    { databaseSettingsConnStr = "postgres://postgres:postgres@localhost:5432/postgres"
    , databaseSettingsPoolsize = 3
    }
  , appMigrationDir = "server/mo-nomz/sql/migrations/"
  }
