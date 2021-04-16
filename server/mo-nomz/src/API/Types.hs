module API.Types where

import ClassyPrelude

import Data.Aeson.TH (deriveJSON)

import Auth (Authorization)
import Json (jsonOptions)
import Types
  ( IngredientId, IngredientName, ReadableQuantity, RecipeId, RecipeLink, RecipeName, Unit, UserId
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

data MergeIngredientRequest = MergeIngredientRequest
  { mergeIngredientRequestIds      :: Set IngredientId
  , mergeIngredientRequestName     :: IngredientName
  , mergeIngredientRequestQuantity :: ReadableQuantity
  , mergeIngredientRequestUnit     :: Unit
  , mergeIngredientRequestActive   :: Bool
  }
  deriving (Eq, Ord, Show)

data DeleteIngredientRequest = DeleteIngredientRequest
  { deleteIngredientRequestIds :: Set IngredientId
  }
  deriving (Eq, Ord, Show)

data ReadableIngredient = ReadableIngredient
  { readableIngredientName     :: IngredientName
  , readableIngredientQuantity :: ReadableQuantity
  , readableIngredientUnit     :: Unit
  , readableIngredientActive   :: Bool
  }
  deriving (Eq, Ord, Show)

data ReadableIngredientAggregate = ReadableIngredientAggregate
  { readableIngredientAggregateIds        :: Set IngredientId
  , readableIngredientAggregateIngredient :: ReadableIngredient
  }
  deriving (Eq, Ord, Show)

data ListIngredientResponse = ListIngredientResponse
  { listIngredientResponseIngredients :: [ReadableIngredientAggregate]
  }
  deriving (Eq, Ord, Show)

data RecipeImportLinkRequest = RecipeImportLinkRequest
  { recipeImportLinkRequestLink :: RecipeLink
  }
  deriving (Eq, Ord, Show)

data RecipeImportBodyRequest = RecipeImportBodyRequest
  { recipeImportBodyRequestName    :: RecipeName
  , recipeImportBodyRequestContent :: Text
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
  { readableRecipeName        :: RecipeName
  , readableRecipeLink        :: Maybe RecipeLink
  , readableRecipeIngredients :: [ReadableIngredient]
  , readableRecipeActive      :: Bool
  }
  deriving (Eq, Ord, Show)

data ListRecipeResponse = ListRecipeResponse
  { listRecipeResponseRecipes :: Map RecipeId ReadableRecipe
  }
  deriving (Eq, Ord, Show)

deriveJSON (jsonOptions "getHealthResponse") ''GetHealthResponse
deriveJSON (jsonOptions "userCreateResponse") ''UserCreateResponse
deriveJSON (jsonOptions "mergeIngredientRequest") ''MergeIngredientRequest
deriveJSON (jsonOptions "deleteIngredientRequest") ''DeleteIngredientRequest
deriveJSON (jsonOptions "readableIngredient") ''ReadableIngredient
deriveJSON (jsonOptions "readableIngredientAggregate") ''ReadableIngredientAggregate
deriveJSON (jsonOptions "listIngredientResponse") ''ListIngredientResponse
deriveJSON (jsonOptions "recipeImportLinkRequest") ''RecipeImportLinkRequest
deriveJSON (jsonOptions "recipeImportBodyRequest") ''RecipeImportBodyRequest
deriveJSON (jsonOptions "updateRecipeRequest") ''UpdateRecipeRequest
deriveJSON (jsonOptions "deleteRecipeRequest") ''DeleteRecipeRequest
deriveJSON (jsonOptions "readableRecipe") ''ReadableRecipe
deriveJSON (jsonOptions "listRecipeResponse") ''ListRecipeResponse
