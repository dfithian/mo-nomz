module Types where

import NomzPrelude

import Chez.Grater.Types (Ingredient(..), IngredientName, Quantity, RecipeName, Step, Unit)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Serialize (Serialize)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Servant.API (FromHttpApiData, ToHttpApiData)

import Serialize.Orphans ()

newtype UserId = UserId { unUserId :: Int }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField, FromHttpApiData, ToHttpApiData)

newtype RecipeLink = RecipeLink { unRecipeLink :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromField, ToField)

newtype GroceryItemId = GroceryItemId { unGroceryItemId :: Int }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField, FromHttpApiData, ToHttpApiData)

newtype IngredientId = IngredientId { unIngredientId :: Int }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField, FromHttpApiData, ToHttpApiData)

newtype RecipeId = RecipeId { unRecipeId :: Int }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField, FromHttpApiData, ToHttpApiData)

data GroceryItem = GroceryItem
  { groceryItemName     :: IngredientName
  , groceryItemQuantity :: Quantity
  , groceryItemUnit     :: Unit
  , groceryItemActive   :: Bool
  }
  deriving (Eq, Ord, Show)

data OrderedGroceryItem = OrderedGroceryItem
  { orderedGroceryItemItem  :: GroceryItem
  , orderedGroceryItemOrder :: Int
  }
  deriving (Eq, Ord, Show)

data OrderedIngredient = OrderedIngredient
  { orderedIngredientIngredient :: Ingredient
  , orderedIngredientOrder      :: Int
  }
  deriving (Eq, Ord, Show)

data Recipe = Recipe
  { recipeName   :: RecipeName
  , recipeLink   :: Maybe RecipeLink
  , recipeActive :: Bool
  , recipeRating :: Int
  , recipeNotes  :: Text
  }
  deriving (Eq, Ord, Show)

data ScrapedRecipe = ScrapedRecipe
  { scrapedRecipeName        :: RecipeName
  , scrapedRecipeIngredients :: [Ingredient]
  , scrapedRecipeSteps       :: [Step]
  } deriving (Eq, Show, Generic)

instance Serialize ScrapedRecipe
