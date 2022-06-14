{-# OPTIONS_GHC -fno-warn-orphans #-}
module Postgres.Orphans where

import NomzPrelude

import Chez.Grater.Scraper.Types (ScrapeName(..), ScrapeVersion(..))
import Chez.Grater.Types (IngredientName(..), Quantity(..), RecipeName(..), Unit(..))
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToField (ToField, toField)

deriving instance FromField RecipeName
deriving instance ToField RecipeName
deriving instance FromField IngredientName
deriving instance ToField IngredientName
deriving instance FromField ScrapeName
deriving instance ToField ScrapeName
deriving instance FromField ScrapeVersion
deriving instance ToField ScrapeVersion

instance FromField Quantity where
  fromField f bs = maybe QuantityMissing Quantity <$> fromField f bs

instance ToField Quantity where
  toField = \case
    Quantity q -> toField (Just q)
    QuantityMissing -> toField (Nothing :: Maybe Double)

instance FromField Unit where
  fromField f bs = maybe UnitMissing Unit <$> fromField f bs

instance ToField Unit where
  toField = \case
    Unit q -> toField (Just q)
    UnitMissing -> toField (Nothing :: Maybe Double)
