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
  , siteScraperRun  :: Scraper Text Text
  }

newtype SiteName = SiteName { unSiteName :: Text }
  deriving (Eq, Ord, Show, IsString, Hashable)

title :: Scraper Text RecipeName
title = RecipeName . strip <$> Scalpel.text "title"
