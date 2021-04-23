module API.Types where

import ClassyPrelude

import Data.Aeson.TH (deriveJSON)

import Auth (Authorization)
import Json (jsonOptions)
import Types
  ( GroceryItemId, IngredientName, ReadableQuantity, ReadableUnit, RecipeId, RecipeLink, RecipeName
  , UserId
  )

data GetHealthResponse = GetHealthResponse
  { getHealthResponseStatus :: Text
  }
  deriving (Eq, Ord, Show)

data UserCreateResponse = UserCreateResponse
  { userCreateResponseUserId   :: UserId
  , userCreateResponseApiToken :: Authorization
  }
  deriving (Eq, Ord, Show)

data MergeGroceryItemRequest = MergeGroceryItemRequest
  { mergeGroceryItemRequestIds      :: Set GroceryItemId
  , mergeGroceryItemRequestName     :: IngredientName
  , mergeGroceryItemRequestQuantity :: ReadableQuantity
  , mergeGroceryItemRequestUnit     :: Maybe ReadableUnit
  , mergeGroceryItemRequestActive   :: Bool
  }
  deriving (Eq, Ord, Show)

data DeleteGroceryItemRequest = DeleteGroceryItemRequest
  { deleteGroceryItemRequestIds :: Set GroceryItemId
  }
  deriving (Eq, Ord, Show)

data ReadableGroceryItem = ReadableGroceryItem
  { readableGroceryItemName     :: IngredientName
  , readableGroceryItemQuantity :: ReadableQuantity
  , readableGroceryItemUnit     :: Maybe ReadableUnit
  , readableGroceryItemActive   :: Bool
  }
  deriving (Eq, Ord, Show)

data ReadableGroceryItemAggregate = ReadableGroceryItemAggregate
  { readableGroceryItemAggregateIds  :: Set GroceryItemId
  , readableGroceryItemAggregateItem :: ReadableGroceryItem
  }
  deriving (Eq, Ord, Show)

data ListGroceryItemResponse = ListGroceryItemResponse
  { listGroceryItemResponseItems :: [ReadableGroceryItemAggregate]
  }
  deriving (Eq, Ord, Show)

data RecipeImportLinkRequest = RecipeImportLinkRequest
  { recipeImportLinkRequestLink :: RecipeLink
  }
  deriving (Eq, Ord, Show)

data IngredientImportBlobRequest = IngredientImportBlobRequest
  { ingredientImportBlobRequestContent :: Text
  }
  deriving (Eq, Ord, Show)

data IngredientImportSingleRequest = IngredientImportSingleRequest
  { ingredientImportSingleRequestName     :: IngredientName
  , ingredientImportSingleRequestQuantity :: ReadableQuantity
  , ingredientImportSingleRequestUnit     :: Maybe ReadableUnit
  }
  deriving (Eq, Ord, Show)

data UpdateRecipeRequest = UpdateRecipeRequest
  { updateRecipeRequestId     :: RecipeId
  , updateRecipeRequestActive :: Bool
  }
  deriving (Eq, Ord, Show)

data DeleteRecipeRequest = DeleteRecipeRequest
  { deleteRecipeRequestIds :: Set RecipeId
  }
  deriving (Eq, Ord, Show)

data ReadableRecipe = ReadableRecipe
  { readableRecipeName   :: RecipeName
  , readableRecipeLink   :: Maybe RecipeLink
  , readableRecipeActive :: Bool
  }
  deriving (Eq, Ord, Show)

data ListRecipeResponse = ListRecipeResponse
  { listRecipeResponseRecipes :: Map RecipeId ReadableRecipe
  }
  deriving (Eq, Ord, Show)

deriveJSON (jsonOptions "getHealthResponse") ''GetHealthResponse
deriveJSON (jsonOptions "userCreateResponse") ''UserCreateResponse
deriveJSON (jsonOptions "mergeGroceryItemRequest") ''MergeGroceryItemRequest
deriveJSON (jsonOptions "deleteGroceryItemRequest") ''DeleteGroceryItemRequest
deriveJSON (jsonOptions "readableGroceryItem") ''ReadableGroceryItem
deriveJSON (jsonOptions "readableGroceryItemAggregate") ''ReadableGroceryItemAggregate
deriveJSON (jsonOptions "listGroceryItemResponse") ''ListGroceryItemResponse
deriveJSON (jsonOptions "recipeImportLinkRequest") ''RecipeImportLinkRequest
deriveJSON (jsonOptions "ingredientImportSingleRequest") ''IngredientImportSingleRequest
deriveJSON (jsonOptions "ingredientImportBlobRequest") ''IngredientImportBlobRequest
deriveJSON (jsonOptions "updateRecipeRequest") ''UpdateRecipeRequest
deriveJSON (jsonOptions "deleteRecipeRequest") ''DeleteRecipeRequest
deriveJSON (jsonOptions "readableRecipe") ''ReadableRecipe
deriveJSON (jsonOptions "listRecipeResponse") ''ListRecipeResponse
