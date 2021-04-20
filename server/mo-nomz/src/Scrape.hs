module Scrape (module Scrape, ScrapedRecipe(..)) where

import ClassyPrelude

import Control.Monad.Except (MonadError, throwError)
import Data.Text (strip)
import Network.URI (URI)
import qualified Data.Attoparsec.Text as Atto
import qualified Text.HTML.Scalpel as Scalpel

import Scraper.Internal.Parser (deduplicateIngredients, ingredientsP, scrubIngredient)
import Scraper.Internal.Site (getSiteScraper)
import Scraper.Internal.Types (ScrapedRecipe(..), title)
import Types (Ingredient)

scrapeUrl :: (MonadIO m, MonadError Text m) => URI -> m ScrapedRecipe
scrapeUrl uri = do
  let scraper = getSiteScraper uri
  liftIO (Scalpel.scrapeURL (show uri) (ScrapedRecipe <$> title <*> scraper)) >>= \case
    Nothing -> throwError "Failed to scrape URL"
    Just x -> pure x

parseIngredients :: (MonadError Text m) => Text -> m [Ingredient]
parseIngredients x = do
  let y = (<> "\n") . unlines . filter (not . null) . map strip . lines $ x
  either (const $ throwError "Failed to parse ingredients") (pure . map scrubIngredient . deduplicateIngredients) $
    Atto.parseOnly ingredientsP y
