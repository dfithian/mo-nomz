module Types where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Internal.Json (jsonOptions)
import Chez.Grater.Readable.Types (ReadableIngredient(..), ReadableQuantity, ReadableStep, ReadableUnit)
import Chez.Grater.Types (Ingredient(..), IngredientName, RecipeName, Step)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.List (zipWith)

newtype RecipeLink = RecipeLink { unRecipeLink :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

data ScrapedRecipe = ScrapedRecipe
  { scrapedRecipeName        :: RecipeName
  , scrapedRecipeIngredients :: [Ingredient]
  , scrapedRecipeSteps       :: [Step]
  } deriving (Eq, Show)

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

data ReadableIngredientLegacy = ReadableIngredientLegacy
  { readableIngredientLegacyName     :: IngredientName
  , readableIngredientLegacyQuantity :: ReadableQuantity
  , readableIngredientLegacyUnit     :: Maybe ReadableUnit
  , readableIngredientLegacyOrder    :: Int
  }
  deriving (Eq, Ord, Show)

data ParseBlobResponseLegacy = ParseBlobResponseLegacy
  { parseBlobResponseLegacyIngredients :: [ReadableIngredientLegacy]
  }
  deriving (Eq, Ord, Show)

data ParseLinkResponseLegacy = ParseLinkResponseLegacy
  { parseLinkResponseLegacyName        :: RecipeName
  , parseLinkResponseLegacyIngredients :: [ReadableIngredientLegacy]
  , parseLinkResponseLegacySteps       :: [ReadableStep]
  }
  deriving (Eq, Ord, Show)

deriveJSON (jsonOptions "parseBlobRequest") ''ParseBlobRequest
deriveJSON (jsonOptions "parseLinkRequest") ''ParseLinkRequest
deriveJSON (jsonOptions "parseBlobResponse") ''ParseBlobResponse
deriveJSON (jsonOptions "parseLinkResponse") ''ParseLinkResponse
deriveJSON (jsonOptions "readableIngredientLegacy") ''ReadableIngredientLegacy
deriveJSON (jsonOptions "parseBlobResponseLegacy") ''ParseBlobResponseLegacy
deriveJSON (jsonOptions "parseLinkResponseLegacy") ''ParseLinkResponseLegacy

toReadableIngredientLegacy :: Int -> ReadableIngredient -> ReadableIngredientLegacy
toReadableIngredientLegacy order ReadableIngredient {..} =
  ReadableIngredientLegacy
    { readableIngredientLegacyName = readableIngredientName
    , readableIngredientLegacyQuantity = readableIngredientQuantity
    , readableIngredientLegacyUnit = readableIngredientUnit
    , readableIngredientLegacyOrder = order
    }

toParseBlobResponseLegacy :: ParseBlobResponse -> ParseBlobResponseLegacy
toParseBlobResponseLegacy ParseBlobResponse {..} =
  ParseBlobResponseLegacy
    { parseBlobResponseLegacyIngredients = zipWith toReadableIngredientLegacy [1 ..] parseBlobResponseIngredients
    }

toParseLinkResponseLegacy :: ParseLinkResponse -> ParseLinkResponseLegacy
toParseLinkResponseLegacy ParseLinkResponse {..} =
  ParseLinkResponseLegacy
    { parseLinkResponseLegacyName = parseLinkResponseName
    , parseLinkResponseLegacyIngredients = zipWith toReadableIngredientLegacy [1 ..] parseLinkResponseIngredients
    , parseLinkResponseLegacySteps = parseLinkResponseSteps
    }
