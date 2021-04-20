module Scrape (module Scrape, ScrapedRecipe(..)) where

import ClassyPrelude

import Control.Monad.Except (MonadError, runExcept, throwError)
import Data.Text (strip)
import Network.URI (URI)
import qualified Data.Attoparsec.Text as Atto
import qualified Text.HTML.Scalpel as Scalpel

import Scraper.Internal.Parser (deduplicateIngredients, ingredientsP, sanitize, scrubIngredient)
import Scraper.Internal.Site (getSiteScrapers)
import Scraper.Internal.Types (ScrapedRecipe(..), title)
import Types (RecipeName(..), Ingredient)

scrapeUrl :: (MonadIO m, MonadError Text m) => URI -> m ScrapedRecipe
scrapeUrl uri = case getSiteScrapers uri of
  Right scraper -> liftIO (Scalpel.scrapeURL (show uri) ((,) <$> title <*> scraper)) >>= \case
    Nothing -> throwError "Failed to scrape URL"
    Just (name, contents) -> ScrapedRecipe name <$> parseIngredients contents
  Left choices -> do
    tags <- liftIO $ Scalpel.fetchTags (show uri)
    let name = fromMaybe (RecipeName "Untitled") $ Scalpel.scrape title tags
        go = \case
          [] -> throwError "Failed to scrape URL"
          choice:more -> case Scalpel.scrape choice tags of
            Nothing -> go choices
            Just contents -> case runExcept (parseIngredients contents) of
              Right ingredients | not (null ingredients) -> pure $ ScrapedRecipe name ingredients
              _ -> go more
    go choices

parseIngredients :: (MonadError Text m) => Text -> m [Ingredient]
parseIngredients x = do
  let y = (<> "\n") . sanitize . unlines . filter (not . null) . map strip . lines $ x
  either (const $ throwError "Failed to parse ingredients") (pure . map scrubIngredient . deduplicateIngredients) $
    Atto.parseOnly ingredientsP y
