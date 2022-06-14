module API.Types where

import NomzPrelude

import Chez.Grater.Internal.Json (jsonOptions)
import Chez.Grater.Readable.Types (ReadableQuantity, ReadableUnit)
import Chez.Grater.Types (IngredientName, RecipeName)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)

import Auth (Authorization)
import Types (GroceryItemId, IngredientId, RecipeId, RecipeLink, UserId)

data GetHealthResponse = GetHealthResponse
  { getHealthResponseStatus           :: Text
  , getHealthResponseVersion          :: Text
  , getHealthResponseStarted          :: UTCTime
  , getHealthResponseFetched          :: UTCTime
  , getHealthResponseUserDay          :: Int
  , getHealthResponseUserWeek         :: Int
  , getHealthResponseUserMonth        :: Int
  , getHealthResponseUserYear         :: Int
  , getHealthResponseCacheSize        :: Int
  , getHealthResponseCacheMostRecent  :: Maybe UTCTime
  , getHealthResponseCacheLeastRecent :: Maybe UTCTime
  }
  deriving (Eq, Ord, Show)

data UserCreateResponse = UserCreateResponse
  { userCreateResponseUserId   :: UserId
  , userCreateResponseApiToken :: Authorization
  }
  deriving (Eq, Ord, Show)

data UserPingRequest = UserPingRequest
  { userPingRequestVersion :: Text
  , userPingRequestTarget  :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data UserPingResponse = UserPingResponse
  { userPingResponseStatus :: Text
  }
  deriving (Eq, Ord, Show)

data ReadableIngredient = ReadableIngredient
  { readableIngredientName     :: IngredientName
  , readableIngredientQuantity :: ReadableQuantity
  , readableIngredientUnit     :: Maybe ReadableUnit
  , readableIngredientOrder    :: Int
  }
  deriving (Eq, Ord, Show)

newtype ReadableStep = ReadableStep { unReadableStep :: Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

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
  , parseLinkResponseSteps       :: [ReadableStep]
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
deriveJSON (jsonOptions "userPingRequest") ''UserPingRequest
deriveJSON (jsonOptions "userPingResponse") ''UserPingResponse
deriveJSON (jsonOptions "readableIngredient") ''ReadableIngredient
deriveJSON (jsonOptions "parseBlobRequest") ''ParseBlobRequest
deriveJSON (jsonOptions "parseLinkRequest") ''ParseLinkRequest
deriveJSON (jsonOptions "parseBlobResponse") ''ParseBlobResponse
deriveJSON (jsonOptions "parseLinkResponse") ''ParseLinkResponse
deriveJSON (jsonOptions "exportGroceryItem") ''ExportGroceryItem
deriveJSON (jsonOptions "exportRecipe") ''ExportRecipe
deriveJSON (jsonOptions "exportIngredient") ''ExportIngredient
deriveJSON (jsonOptions "exportResponse") ''ExportResponse
