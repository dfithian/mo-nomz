module API.Types where

import ClassyPrelude

import Data.Aeson.TH (deriveJSON)

import Auth (Authorization)
import Json (jsonOptions)
import Types
  ( GroceryItemId, IngredientId, IngredientName, ReadableQuantity, ReadableUnit, RecipeId
  , RecipeLink, RecipeName, UserId
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

data UpdateGroceryItemRequest = UpdateGroceryItemRequest
  { updateGroceryItemRequestId       :: GroceryItemId
  , updateGroceryItemRequestName     :: IngredientName
  , updateGroceryItemRequestQuantity :: ReadableQuantity
  , updateGroceryItemRequestUnit     :: Maybe ReadableUnit
  , updateGroceryItemRequestActive   :: Bool
  , updateGroceryItemRequestOrder    :: Int
  }
  deriving (Eq, Ord, Show)

data MergeGroceryItemRequest = MergeGroceryItemRequest
  { mergeGroceryItemRequestIds      :: Set GroceryItemId
  , mergeGroceryItemRequestName     :: IngredientName
  , mergeGroceryItemRequestQuantity :: ReadableQuantity
  , mergeGroceryItemRequestUnit     :: Maybe ReadableUnit
  , mergeGroceryItemRequestActive   :: Bool
  , mergeGroceryItemRequestOrder    :: Int
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
  , readableGroceryItemOrder    :: Int
  }
  deriving (Eq, Ord, Show)

data ListGroceryItemResponse = ListGroceryItemResponse
  { listGroceryItemResponseItems :: Map GroceryItemId ReadableGroceryItem
  }
  deriving (Eq, Ord, Show)

data RecipeImportLinkRequest = RecipeImportLinkRequest
  { recipeImportLinkRequestLink   :: RecipeLink
  , recipeImportLinkRequestActive :: Bool
  }
  deriving (Eq, Ord, Show)

data GroceryImportBlobRequest = GroceryImportBlobRequest
  { groceryImportBlobRequestContent :: Text
  , groceryImportBlobRequestName    :: Maybe RecipeName
  }
  deriving (Eq, Ord, Show)

data GroceryImportSingle = GroceryImportSingle
  { groceryImportSingleName     :: IngredientName
  , groceryImportSingleQuantity :: ReadableQuantity
  , groceryImportSingleUnit     :: Maybe ReadableUnit
  }
  deriving (Eq, Ord, Show)

data GroceryImportListRequest = GroceryImportListRequest
  { groceryImportListRequestItems :: [GroceryImportSingle]
  }
  deriving (Eq, Ord, Show)

data UpdateRecipeRequest = UpdateRecipeRequest
  { updateRecipeRequestId     :: RecipeId
  , updateRecipeRequestActive :: Bool
  , updateRecipeRequestRating :: Maybe Int
  , updateRecipeRequestNotes  :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data UpdateRecipeIngredientsRequest = UpdateRecipeIngredientsRequest
  { updateRecipeIngredientsRequestId      :: RecipeId
  , updateRecipeIngredientsRequestDeletes :: Set IngredientId
  , updateRecipeIngredientsRequestAdds    :: [ReadableIngredient]
  }
  deriving (Eq, Ord, Show)

data DeleteRecipeRequest = DeleteRecipeRequest
  { deleteRecipeRequestIds :: Set RecipeId
  }
  deriving (Eq, Ord, Show)

data ReadableIngredient = ReadableIngredient
  { readableIngredientName     :: IngredientName
  , readableIngredientQuantity :: ReadableQuantity
  , readableIngredientUnit     :: Maybe ReadableUnit
  , readableIngredientOrder    :: Int
  }
  deriving (Eq, Ord, Show)

data ReadableRecipe = ReadableRecipe
  { readableRecipeName          :: RecipeName
  , readableRecipeLink          :: Maybe RecipeLink
  , readableRecipeActive        :: Bool
  , readableRecipeRating        :: Int
  , readableRecipeNotes         :: Text
  , readableRecipeIngredients   :: [ReadableIngredient]
  , readableRecipeIngredientsV2 :: Map IngredientId ReadableIngredient
  }
  deriving (Eq, Ord, Show)

data ListRecipeResponse = ListRecipeResponse
  { listRecipeResponseRecipes :: Map RecipeId ReadableRecipe
  }
  deriving (Eq, Ord, Show)

deriveJSON (jsonOptions "getHealthResponse") ''GetHealthResponse
deriveJSON (jsonOptions "userCreateResponse") ''UserCreateResponse
deriveJSON (jsonOptions "updateGroceryItemRequest") ''UpdateGroceryItemRequest
deriveJSON (jsonOptions "mergeGroceryItemRequest") ''MergeGroceryItemRequest
deriveJSON (jsonOptions "deleteGroceryItemRequest") ''DeleteGroceryItemRequest
deriveJSON (jsonOptions "readableGroceryItem") ''ReadableGroceryItem
deriveJSON (jsonOptions "listGroceryItemResponse") ''ListGroceryItemResponse
deriveJSON (jsonOptions "recipeImportLinkRequest") ''RecipeImportLinkRequest
deriveJSON (jsonOptions "groceryImportSingle") ''GroceryImportSingle
deriveJSON (jsonOptions "groceryImportListRequest") ''GroceryImportListRequest
deriveJSON (jsonOptions "groceryImportBlobRequest") ''GroceryImportBlobRequest
deriveJSON (jsonOptions "updateRecipeRequest") ''UpdateRecipeRequest
deriveJSON (jsonOptions "updateRecipeIngredientsRequest") ''UpdateRecipeIngredientsRequest
deriveJSON (jsonOptions "deleteRecipeRequest") ''DeleteRecipeRequest
deriveJSON (jsonOptions "readableIngredient") ''ReadableIngredient
deriveJSON (jsonOptions "readableRecipe") ''ReadableRecipe
deriveJSON (jsonOptions "listRecipeResponse") ''ListRecipeResponse
