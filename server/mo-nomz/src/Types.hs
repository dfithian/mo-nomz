module Types where

import ClassyPrelude

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson.TH (deriveJSON)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Servant.API (FromHttpApiData, ToHttpApiData)

import Json (jsonOptions)

newtype IngredientName = IngredientName { unIngredientName :: Text }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField)

newtype Quantity = Quantity { unQuantity :: Double }
  deriving (Eq, Ord, Show, Num, Fractional, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField)

newtype RecipeName = RecipeName { unRecipeName :: Text }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField)

newtype RecipeLink = RecipeLink { unRecipeLink :: Text }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField)

newtype RecipeId = RecipeId { unRecipeId :: Int }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField, FromHttpApiData, ToHttpApiData)

newtype Unit = Unit { unUnit :: Text }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField)

data Ingredient = Ingredient
  { ingredientName     :: IngredientName
  , ingredientQuantity :: Quantity
  , ingredientUnit     :: Unit
  }
  deriving (Eq, Ord, Show)

data Recipe = Recipe
  { recipeName        :: RecipeName
  , recipeIngredients :: [Ingredient]
  -- ^ The ingredients in the recipe.
  , recipeLink        :: Maybe RecipeLink
  -- ^ The link to the recipe, if it exists.
  }
  deriving (Eq, Ord, Show)

deriveJSON (jsonOptions "ingredient") ''Ingredient
deriveJSON (jsonOptions "recipe") ''Recipe

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
