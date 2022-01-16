module Types where

import ClassyPrelude

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson.TH (deriveJSON)
import Data.CaseInsensitive (CI)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import Servant.API (FromHttpApiData, ToHttpApiData)

import CI.Orphans ()
import Json (jsonOptions)

newtype UserId = UserId { unUserId :: Int }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField, FromHttpApiData, ToHttpApiData)

newtype IngredientName = IngredientName { unIngredientName :: CI Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromField, ToField)

newtype RecipeName = RecipeName { unRecipeName :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromField, ToField)

data RawQuantity
  = RawQuantity Double
  | RawQuantityWord (CI Text)
  | RawQuantityMissing
  deriving (Eq, Ord, Show)

data Quantity
  = Quantity Double
  | QuantityMissing
  deriving (Eq, Ord, Show)

quantityToValue :: Quantity -> Double
quantityToValue = \case
  Quantity x -> x
  QuantityMissing -> 1

newtype RecipeLink = RecipeLink { unRecipeLink :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromField, ToField)

data RawUnit
  = RawUnit (CI Text)
  | RawUnitMissing
  deriving (Eq, Ord, Show)

data Unit
  = Unit (CI Text)
  | UnitMissing
  deriving (Eq, Ord, Show)

newtype GroceryItemId = GroceryItemId { unGroceryItemId :: Int }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromField, ToField, FromHttpApiData, ToHttpApiData)

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

newtype ReadableUnit = ReadableUnit { unReadableUnit :: CI Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromField, ToField)

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

data Ingredient = Ingredient
  { ingredientName     :: IngredientName
  , ingredientQuantity :: Quantity
  , ingredientUnit     :: Unit
  }
  deriving (Eq, Ord, Show)

data OrderedIngredient = OrderedIngredient
  { orderedIngredientIngredient :: Ingredient
  , orderedIngredientOrder      :: Int
  }
  deriving (Eq, Ord, Show)

data RawIngredient = RawIngredient
  { rawIngredientName     :: IngredientName
  , rawIngredientQuantity :: RawQuantity
  , rawIngredientUnit     :: RawUnit
  }
  deriving (Eq, Ord, Show)

newtype Step = Step { unStep :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

data Recipe = Recipe
  { recipeName   :: RecipeName
  , recipeLink   :: Maybe RecipeLink
  , recipeActive :: Bool
  , recipeRating :: Int
  , recipeNotes  :: Text
  }
  deriving (Eq, Ord, Show)

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

deriveJSON (jsonOptions "readableFraction") ''ReadableFraction
deriveJSON (jsonOptions "readableQuantity") ''ReadableQuantity

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (x, y, z, w) = f x y z w

mapError :: (MonadError e2 m) => (e1 -> e2) -> ExceptT e1 m a -> m a
mapError f = either (throwError . f) pure <=< runExceptT

pinch, teaspoon, tablespoon, cup, ounce, box, pound, splash, sprinkle, whole
  , milliliter, liter, milligram, gram :: Unit
pinch = Unit "pinch"
teaspoon = Unit "tsp"
tablespoon = Unit "tbsp"
cup = Unit "cup"
ounce = Unit "oz"
box = Unit "box"
pound = Unit "pound"
splash = Unit "splash"
sprinkle = Unit "sprinkle"
whole = Unit "whole"
milliliter = Unit "ml"
liter = Unit "l"
milligram = Unit "mg"
gram = Unit "g"

ingredientToGroceryItem :: Bool -> Ingredient -> GroceryItem
ingredientToGroceryItem active Ingredient {..} = GroceryItem
  { groceryItemName = ingredientName
  , groceryItemQuantity = ingredientQuantity
  , groceryItemUnit = ingredientUnit
  , groceryItemActive = active
  }

groceryItemToIngredient :: GroceryItem -> Ingredient
groceryItemToIngredient GroceryItem {..} = Ingredient
  { ingredientName = groceryItemName
  , ingredientQuantity = groceryItemQuantity
  , ingredientUnit = groceryItemUnit
  }

instance Num Quantity where
  QuantityMissing + QuantityMissing = QuantityMissing
  x + y = Quantity $ quantityToValue x + quantityToValue y

  QuantityMissing * QuantityMissing = QuantityMissing
  x * y = Quantity $ quantityToValue x * quantityToValue y

  abs = \case
    Quantity x -> Quantity $ abs x
    QuantityMissing -> QuantityMissing

  signum = \case
    Quantity x -> Quantity $ signum x
    QuantityMissing -> QuantityMissing

  fromInteger = Quantity . fromInteger

  negate = \case
    Quantity x -> Quantity $ negate x
    QuantityMissing -> QuantityMissing

instance Fractional Quantity where
  fromRational = Quantity . fromRational

  QuantityMissing / QuantityMissing = QuantityMissing
  x / y = Quantity $ quantityToValue x / quantityToValue y
