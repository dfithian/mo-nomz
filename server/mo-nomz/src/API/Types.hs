module API.Types where

import ClassyPrelude

import Data.Aeson.TH (deriveJSON)

import Json (jsonOptions)
import Types (IngredientId, IngredientName, ReadableQuantity, RecipeLink, Unit, UserId, Username)

data UserCreateRequest = UserCreateRequest
  { userCreateRequestUsername :: Username
  }
  deriving (Eq, Ord, Show)

data UserCreateResponse = UserCreateResponse
  { userCreateResponseUserId :: UserId
  }
  deriving (Eq, Ord, Show)

data RecipeImportLinkRequest = RecipeImportLinkRequest
  { recipeImportLinkRequestLink :: RecipeLink
  }
  deriving (Eq, Ord, Show)

data MergeIngredientRequest = MergeIngredientRequest
  { mergeIngredientRequestIds      :: Set IngredientId
  , mergeIngredientRequestName     :: IngredientName
  , mergeIngredientRequestQuantity :: ReadableQuantity
  , mergeIngredientRequestUnit     :: Unit
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

deriveJSON (jsonOptions "userCreateRequest") ''UserCreateRequest
deriveJSON (jsonOptions "userCreateResponse") ''UserCreateResponse
deriveJSON (jsonOptions "recipeImportLinkRequest") ''RecipeImportLinkRequest
deriveJSON (jsonOptions "mergeIngredientRequest") ''MergeIngredientRequest
deriveJSON (jsonOptions "deleteIngredientRequest") ''DeleteIngredientRequest
deriveJSON (jsonOptions "readableIngredient") ''ReadableIngredient
deriveJSON (jsonOptions "readableIngredientAggregate") ''ReadableIngredientAggregate
deriveJSON (jsonOptions "listIngredientResponse") ''ListIngredientResponse
