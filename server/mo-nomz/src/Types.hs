module Types where

import ClassyPrelude

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson.TH (deriveJSON)
import Data.CaseInsensitive (CI)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Servant.API (FromHttpApiData, ToHttpApiData)

import CI.Orphans ()
import Json (jsonOptions)

newtype UserId = UserId { unUserId :: Int }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField, FromHttpApiData, ToHttpApiData)

newtype Username = Username { unUsername :: CI Text }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField)

newtype IngredientName = IngredientName { unIngredientName :: CI Text }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField)

newtype RecipeName = RecipeName { unRecipeName :: Text }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField)

data RawQuantity
  = RawQuantityPure Quantity
  | RawQuantityWord (CI Text)
  | RawQuantityMissing
  deriving (Eq, Ord, Show)

newtype Quantity = Quantity { unQuantity :: Double }
  deriving (Eq, Ord, Show, Num, Fractional, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField)

newtype RecipeLink = RecipeLink { unRecipeLink :: Text }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField)

data RawUnit
  = RawUnitWord (CI Text)
  | RawUnitMissing
  deriving (Eq, Ord, Show)

newtype Unit = Unit { unUnit :: CI Text }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField)

newtype IngredientId = IngredientId { unIngredientId :: Int }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField, FromHttpApiData, ToHttpApiData)

newtype RecipeId = RecipeId { unRecipeId :: Int }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField, FromHttpApiData, ToHttpApiData)

data ReadableFraction = ReadableFraction
  { readableFractionNumerator   :: Int
  , readableFractionDenominator :: Int
  }
  deriving (Eq, Show, Ord)

data ReadableQuantity = ReadableQuantity
  { readableQuantityWhole    :: Maybe Int
  , readableQuantityFraction :: Maybe ReadableFraction
  }
  deriving (Eq, Show, Ord)

data User = User
  { userUsername :: Username
  }
  deriving (Eq, Ord, Show)

data Ingredient = Ingredient
  { ingredientName     :: IngredientName
  , ingredientQuantity :: Quantity
  , ingredientUnit     :: Unit
  }
  deriving (Eq, Ord, Show)

data RawIngredient = RawIngredient
  { rawIngredientName     :: IngredientName
  , rawIngredientQuantity :: RawQuantity
  , rawIngredientUnit     :: RawUnit
  }
  deriving (Eq, Ord, Show)

data RecipeIngredient = RecipeIngredient
  { recipeIngredientName     :: IngredientName
  , recipeIngredientQuantity :: Quantity
  , recipeIngredientUnit     :: Unit
  }
  deriving (Eq, Ord, Show)

data Recipe = Recipe
  { recipeName        :: RecipeName
  , recipeLink        :: RecipeLink
  , recipeIngredients :: [RecipeIngredient]
  , recipeActive      :: Bool
  }
  deriving (Eq, Ord, Show)

deriveJSON (jsonOptions "readableFraction") ''ReadableFraction
deriveJSON (jsonOptions "readableQuantity") ''ReadableQuantity
deriveJSON (jsonOptions "user") ''User
deriveJSON (jsonOptions "ingredient") ''Ingredient
deriveJSON (jsonOptions "recipeIngredient") ''RecipeIngredient
deriveJSON (jsonOptions "recipe") ''Recipe

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

mapError :: (MonadError e2 m) => (e1 -> e2) -> ExceptT e1 m a -> m a
mapError f = either (throwError . f) pure <=< runExceptT

pinch, teaspoon, tablespoon, cup, ounce :: Unit
pinch = Unit "pinch"
teaspoon = Unit "tsp"
tablespoon = Unit "tbsp"
cup = Unit "cup"
ounce = Unit "oz"
