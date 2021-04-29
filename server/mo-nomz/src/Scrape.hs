module Scrape (module Scrape, ScrapedRecipe(..)) where

import ClassyPrelude

import Control.Monad.Except (MonadError, throwError)
import Network.URI (URI)
import qualified Text.HTML.Scalpel as Scalpel

import Foundation (HasManager, manager)
import Parser (parseIngredients)
import Scraper.Internal.Site (getSiteScrapers)
import Scraper.Internal.Types (ScrapedRecipe(..), title)
import Types (RecipeName(..))

scrapeUrl :: (HasManager r, MonadIO m, MonadError Text m, MonadReader r m) => URI -> m ScrapedRecipe
scrapeUrl uri = do
  cfg <- Scalpel.Config Scalpel.defaultDecoder . Just <$> asks manager
  case getSiteScrapers uri of
    Right scraper -> liftIO (Scalpel.scrapeURLWithConfig cfg (show uri) ((,) <$> title <*> scraper)) >>= \case
      Nothing -> throwError "Failed to scrape known URL"
      Just (name, contents) -> either throwError (pure . ScrapedRecipe name) $ parseIngredients contents
    Left choices -> do
      tags <- liftIO $ Scalpel.fetchTagsWithConfig cfg (show uri)
      let name = fromMaybe (RecipeName "Untitled") $ Scalpel.scrape title tags
          go = \case
            [] -> throwError "Failed to scrape URL from defaults"
            choice:more -> case Scalpel.scrape choice tags of
              Nothing -> go choices
              Just contents -> case parseIngredients contents of
                Right ingredients | not (null ingredients) -> pure $ ScrapedRecipe name ingredients
                _ -> go more
      go choices
