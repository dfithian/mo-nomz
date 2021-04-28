module Scrape (module Scrape, ScrapedRecipe(..)) where

import ClassyPrelude

import Control.Monad.Except (MonadError, throwError)
import Network.URI (URI)
import qualified Text.HTML.Scalpel as Scalpel

import Parser (parseIngredients)
import Scraper.Internal.Site (getSiteScrapers)
import Scraper.Internal.Types (ScrapedRecipe(..), title)
import Types (RecipeName(..))

scrapeUrl :: (MonadIO m, MonadError Text m) => URI -> m ScrapedRecipe
scrapeUrl uri = case getSiteScrapers uri of
  Right scraper -> liftIO (Scalpel.scrapeURL (show uri) ((,) <$> title <*> scraper)) >>= \case
    Nothing -> throwError "Failed to scrape URL"
    Just (name, contents) -> either throwError (pure . ScrapedRecipe name) $ parseIngredients contents
  Left choices -> do
    tags <- liftIO $ Scalpel.fetchTags (show uri)
    let name = fromMaybe (RecipeName "Untitled") $ Scalpel.scrape title tags
        go = \case
          [] -> throwError "Failed to scrape URL"
          choice:more -> case Scalpel.scrape choice tags of
            Nothing -> go choices
            Just contents -> case parseIngredients contents of
              Right ingredients | not (null ingredients) -> pure $ ScrapedRecipe name ingredients
              _ -> go more
    go choices
