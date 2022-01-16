module API.Types where

import ClassyPrelude

import Data.Aeson.TH (deriveJSON)

import Auth (Authorization)
import Json (jsonOptions)
import Types
  ( GroceryItemId, IngredientId, IngredientName, ReadableQuantity, ReadableUnit, RecipeId
  , RecipeLink, RecipeName, Step, UserId
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
  , groceryImportBlobRequestLink    :: Maybe RecipeLink
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

data ReadableIngredientV1 = ReadableIngredientV1
  { readableIngredientV1Name     :: IngredientName
  , readableIngredientV1Quantity :: ReadableQuantity
  , readableIngredientV1Unit     :: Maybe ReadableUnit
  }
  deriving (Eq, Ord, Show)

data ReadableIngredient = ReadableIngredient
  { readableIngredientName     :: IngredientName
  , readableIngredientQuantity :: ReadableQuantity
  , readableIngredientUnit     :: Maybe ReadableUnit
  , readableIngredientOrder    :: Int
  }
  deriving (Eq, Ord, Show)

data ReadableRecipeV1 = ReadableRecipeV1
  { readableRecipeV1Name        :: RecipeName
  , readableRecipeV1Link        :: Maybe RecipeLink
  , readableRecipeV1Active      :: Bool
  , readableRecipeV1Rating      :: Int
  , readableRecipeV1Notes       :: Text
  , readableRecipeV1Ingredients :: [ReadableIngredientV1]
  }
  deriving (Eq, Ord, Show)

data ReadableRecipe = ReadableRecipe
  { readableRecipeName        :: RecipeName
  , readableRecipeLink        :: Maybe RecipeLink
  , readableRecipeActive      :: Bool
  , readableRecipeRating      :: Int
  , readableRecipeNotes       :: Text
  , readableRecipeIngredients :: Map IngredientId ReadableIngredient
  }
  deriving (Eq, Ord, Show)

data ListRecipeResponseV1 = ListRecipeResponseV1
  { listRecipeResponseV1Recipes :: Map RecipeId ReadableRecipeV1
  }
  deriving (Eq, Ord, Show)

data ListRecipeResponse = ListRecipeResponse
  { listRecipeResponseRecipes :: Map RecipeId ReadableRecipe
  }
  deriving (Eq, Ord, Show)

data ParseBlobRequest = ParseBlobRequest
  { parseBlobRequestContent :: Text
  }
  deriving (Eq, Ord, Show)

data ParseLinkRequest = ParseLinkRequest
  { parseLinkRequestLink :: RecipeLink
  }
  deriving (Eq, Ord, Show)

data ParseBlobResponse = ParseBlobResponse
  { parseBlobResponseIngredients :: [ReadableIngredient]
  }
  deriving (Eq, Ord, Show)

data ParseLinkResponse = ParseLinkResponse
  { parseLinkResponseName        :: RecipeName
  , parseLinkResponseIngredients :: [ReadableIngredient]
  , parseLinkResponseSteps       :: [Step]
  }
  deriving (Eq, Ord, Show)

data ExportGroceryItem = ExportGroceryItem
  { exportGroceryItemName     :: IngredientName
  , exportGroceryItemQuantity :: ReadableQuantity
  , exportGroceryItemUnit     :: Maybe ReadableUnit
  , exportGroceryItemActive   :: Bool
  , exportGroceryItemOrder    :: Int
  }
  deriving (Eq, Ord, Show)

data ExportRecipe = ExportRecipe
  { exportRecipeName   :: RecipeName
  , exportRecipeLink   :: Maybe RecipeLink
  , exportRecipeActive :: Bool
  , exportRecipeRating :: Int
  , exportRecipeNotes  :: Text
  }
  deriving (Eq, Ord, Show)

data ExportIngredient = ExportIngredient
  { exportIngredientGroceryItemId :: Maybe GroceryItemId
  , exportIngredientRecipeId      :: Maybe RecipeId
  , exportIngredientName          :: IngredientName
  , exportIngredientQuantity      :: ReadableQuantity
  , exportIngredientUnit          :: Maybe ReadableUnit
  , exportIngredientOrder         :: Int
  }
  deriving (Eq, Ord, Show)

data ExportResponse = ExportResponse
  { exportResponseGroceries   :: Map GroceryItemId ExportGroceryItem
  , exportResponseRecipes     :: Map RecipeId ExportRecipe
  , exportResponseIngredients :: Map IngredientId ExportIngredient
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
deriveJSON (jsonOptions "groceryImportBlobRequest") ''GroceryImportBlobRequest
deriveJSON (jsonOptions "updateRecipeRequest") ''UpdateRecipeRequest
deriveJSON (jsonOptions "updateRecipeIngredientsRequest") ''UpdateRecipeIngredientsRequest
deriveJSON (jsonOptions "deleteRecipeRequest") ''DeleteRecipeRequest
deriveJSON (jsonOptions "readableIngredientV1") ''ReadableIngredientV1
deriveJSON (jsonOptions "readableIngredient") ''ReadableIngredient
deriveJSON (jsonOptions "readableRecipeV1") ''ReadableRecipeV1
deriveJSON (jsonOptions "readableRecipe") ''ReadableRecipe
deriveJSON (jsonOptions "listRecipeResponseV1") ''ListRecipeResponseV1
deriveJSON (jsonOptions "listRecipeResponse") ''ListRecipeResponse
deriveJSON (jsonOptions "parseBlobRequest") ''ParseBlobRequest
deriveJSON (jsonOptions "parseLinkRequest") ''ParseLinkRequest
deriveJSON (jsonOptions "parseBlobResponse") ''ParseBlobResponse
deriveJSON (jsonOptions "parseLinkResponse") ''ParseLinkResponse
deriveJSON (jsonOptions "exportGroceryItem") ''ExportGroceryItem
deriveJSON (jsonOptions "exportRecipe") ''ExportRecipe
deriveJSON (jsonOptions "exportIngredient") ''ExportIngredient
deriveJSON (jsonOptions "exportResponse") ''ExportResponse
