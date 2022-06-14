{-# OPTIONS_GHC -fno-warn-orphans #-}
module Serialize.Orphans where

import NomzPrelude

import Chez.Grater.Types
  ( Ingredient(..), IngredientName(..), Quantity(..), RecipeName(..), Step(..), Unit(..)
  )
import Data.Serialize (Serialize, get, put)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text

deriving instance Generic RecipeName
deriving instance Generic IngredientName
deriving instance Generic Quantity
deriving instance Generic Unit
deriving instance Generic Ingredient
deriving instance Generic Step

instance Serialize Text where
  put = put . Text.unpack
  get = Text.pack <$> get

instance (Serialize a, CI.FoldCase a) => Serialize (CI a) where
  put = put . CI.original
  get = CI.mk <$> get

instance Serialize RecipeName
instance Serialize IngredientName
instance Serialize Quantity
instance Serialize Unit
instance Serialize Ingredient
instance Serialize Step
