module Scraper.Internal.Types where

import ClassyPrelude
import Data.Serialize (Serialize)
import Data.Text (strip)
import Text.HTML.Scalpel (Scraper)
import qualified Text.HTML.Scalpel as Scalpel

import Types (RecipeName(..), Ingredient, Step)

data ScrapedRecipe = ScrapedRecipe
  { scrapedRecipeName        :: RecipeName
  , scrapedRecipeIngredients :: [Ingredient]
  , scrapedRecipeSteps       :: [Step]
  } deriving (Eq, Show, Generic)

data IngredientScraper = IngredientScraper
  { ingredientScraperName    :: Text
  , ingredientScraperTest    :: Scraper Text Bool
  , ingredientScraperRun     :: Scraper Text [UnparsedIngredient]
  }

data StepScraper = StepScraper
  { stepScraperName    :: Text
  , stepScraperTest    :: Scraper Text Bool
  , stepScraperRun     :: Scraper Text [UnparsedStep]
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
