module Settings where

import ClassyPrelude

import Data.Aeson ((.:), FromJSON, parseJSON, withObject)
import Database.PostgreSQL.Config (PostgresConf)

data AppSettings = AppSettings
  { appPort         :: Int -- ^ The port to serve the application on.
  , appPostgresConf :: PostgresConf -- ^ Postgres conf.
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \obj ->
    AppSettings
      <$> obj .: "port"
      <*> obj .: "database"
