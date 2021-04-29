module Scraper.Internal.Types where

import ClassyPrelude
import Data.Text (strip)
import Text.HTML.Scalpel (Scraper)
import qualified Text.HTML.Scalpel as Scalpel

import Types (RecipeName(..), Ingredient)

data ScrapedRecipe = ScrapedRecipe
  { scrapedRecipeName        :: RecipeName
  , scrapedRecipeIngredients :: [Ingredient]
  } deriving (Eq, Show)

data SiteScraper = SiteScraper
  { siteScraperName :: Text
  , siteScraperRun  :: Scraper Text [UnparsedIngredient]
  }

newtype SiteName = SiteName { unSiteName :: Text }
  deriving (Eq, Ord, Show, IsString, Hashable)

newtype UnparsedQuantity = UnparsedQuantity { unUnparsedQuantity :: Text }
  deriving (Eq, Ord, Show)

newtype UnparsedUnit = UnparsedUnit { unUnparsedUnit :: Text }
  deriving (Eq, Ord, Show)

newtype UnparsedQuantityUnit = UnparsedQuantityUnit { unUnparsedQuantityUnit :: Text }
  deriving (Eq, Ord, Show)

data UnparsedIngredient
  = UnparsedIngredientRaw Text
  | UnparsedIngredientStructured1 UnparsedQuantity Text
  | UnparsedIngredientStructured2 UnparsedQuantity UnparsedUnit Text
  | UnparsedIngredientStructured3 UnparsedQuantityUnit Text
  deriving (Eq, Ord, Show)

title :: Scraper Text RecipeName
title = RecipeName . strip <$> Scalpel.text "title"
