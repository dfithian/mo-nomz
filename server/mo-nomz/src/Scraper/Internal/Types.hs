module Scraper.Internal.Types where

import ClassyPrelude
import Data.Text (strip)
import Text.HTML.Scalpel (Scraper)
import qualified Text.HTML.Scalpel as Scalpel

import Types (RecipeName(..), Ingredient, Step)

data ScrapedRecipe = ScrapedRecipe
  { scrapedRecipeName        :: RecipeName
  , scrapedRecipeIngredients :: [Ingredient]
  , scrapedRecipeSteps       :: [Step]
  } deriving (Eq, Show)

data SiteScraper = SiteScraper
  { siteScraperName :: Text
  , siteScraperTest :: Scraper Text Bool
  , siteScraperRun  :: Scraper Text ([UnparsedIngredient], [UnparsedStep])
  }

newtype SiteName = SiteName { unSiteName :: Text }
  deriving (Eq, Ord, Show, IsString, Hashable)

data UnparsedIngredient = UnparsedIngredientRaw Text
  deriving (Eq, Ord, Show)

data UnparsedStep = UnparsedStep Text
  deriving (Eq, Ord, Show)

title :: Scraper Text RecipeName
title = RecipeName . strip <$> Scalpel.text "title"
