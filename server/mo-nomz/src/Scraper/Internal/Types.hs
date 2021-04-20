module Scraper.Internal.Types where

import ClassyPrelude
import Data.Text (strip)
import Text.HTML.Scalpel (AttributePredicate, Scraper, (//), (@:))
import qualified Text.HTML.Scalpel as Scalpel

import Types (RecipeName(..))

data ScrapedRecipe = ScrapedRecipe
  { scrapedRecipeTitle    :: RecipeName
  , scrapedRecipeContents :: Text
  } deriving (Eq, Show)

newtype SiteName = SiteName { unSiteName :: Text }
  deriving (Eq, Ord, Show, IsString)

title :: Scraper Text RecipeName
title = RecipeName . strip <$> Scalpel.text "title"

containsIngredientClass :: AttributePredicate
containsIngredientClass = Scalpel.match $ \attributeKey attributeValue -> case attributeKey of
  "class" -> "ingredient" `isInfixOf` toLower attributeValue
  _ -> False

defaultScraper :: Scraper Text Text
defaultScraper = unlines . map unlines <$> Scalpel.chroots (Scalpel.AnyTag @: [containsIngredientClass] // "li") (Scalpel.texts Scalpel.anySelector)
