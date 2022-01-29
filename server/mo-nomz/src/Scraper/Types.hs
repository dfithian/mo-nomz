module Scraper.Types where

import ClassyPrelude

import Data.Serialize (Serialize)
import Data.Text (strip)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Text.HTML.Scalpel (Scraper)
import qualified Text.HTML.Scalpel as Scalpel

import Types (RecipeName(..), Ingredient, Step)

data ScrapedInfo
  = ScrapedInfoIngredient (ScrapeInfo Ingredient)
  | ScrapedInfoIngredientStep (ScrapeInfo Ingredient) (ScrapeInfo Step)
  deriving (Eq, Ord, Show)

newtype ScrapeName = ScrapeName { unScrapeName :: Text }
  deriving (Eq, Ord, Show, Generic, FromField, ToField)

newtype ScrapeVersion = ScrapeVersion { unScrapeVersion :: Int }
  deriving (Eq, Ord, Show, Generic, FromField, ToField)

data ScrapeInfo a = ScrapeInfo
  { scrapeInfoName    :: ScrapeName
  , scrapeInfoVersion :: ScrapeVersion
  }
  deriving (Eq, Ord, Show, Generic)

data ScrapedRecipe = ScrapedRecipe
  { scrapedRecipeName        :: RecipeName
  , scrapedRecipeIngredients :: [Ingredient]
  , scrapedRecipeSteps       :: [Step]
  } deriving (Eq, Show, Generic)

data IngredientScraper = IngredientScraper
  { ingredientScraperInfo :: ScrapeInfo Ingredient
  , ingredientScraperTest :: Scraper Text Bool
  , ingredientScraperRun  :: Scraper Text [UnparsedIngredient]
  }

data StepScraper = StepScraper
  { stepScraperInfo :: ScrapeInfo Step
  , stepScraperTest :: Scraper Text Bool
  , stepScraperRun  :: Scraper Text [UnparsedStep]
  }

newtype SiteName = SiteName { unSiteName :: Text }
  deriving (Eq, Ord, Show, IsString, Hashable)

data UnparsedIngredient = UnparsedIngredientRaw Text
  deriving (Eq, Ord, Show)

data UnparsedStep = UnparsedStepRaw Text
  deriving (Eq, Ord, Show)

title :: Scraper Text RecipeName
title = RecipeName . strip <$> Scalpel.text "title"

instance Serialize ScrapedRecipe

inception :: ScrapeVersion
inception = ScrapeVersion 1
