module Scrape (module Scrape, ScrapedRecipe(..)) where

import ClassyPrelude

import Control.Monad.Except (MonadError, throwError)
import Data.Text (replace)
import Network.URI (URI, uriAuthority, uriRegName)
import qualified Text.HTML.Scalpel as Scalpel

import Foundation (HasManager, manager)
import Parser (parseIngredients)
import Scraper.Internal.Site (allSiteScrapers, siteScrapers)
import Scraper.Internal.Types (ScrapedRecipe(..), SiteName(..), SiteScraper(..), title)
import Types (RecipeName(..))

scrapeUrl :: (HasManager r, MonadIO m, MonadError Text m, MonadReader r m) => URI -> m ScrapedRecipe
scrapeUrl uri = do
  cfg <- Scalpel.Config Scalpel.defaultDecoder . Just <$> asks manager
  tags <- liftIO $ Scalpel.fetchTagsWithConfig cfg (show uri)
  let domainMay = SiteName . replace "www." "" . pack . uriRegName <$> uriAuthority uri
      name = fromMaybe (RecipeName "Untitled") $ Scalpel.scrape title tags
      runParser contents = case parseIngredients contents of
        Right ingredients | not (null ingredients) -> pure $ ScrapedRecipe name ingredients
        _ -> Nothing
      runScraper scraper = runParser =<< Scalpel.scrape scraper tags
      go SiteScraper {..} acc = case Scalpel.scrape siteScraperTest tags of
        Just True -> case runScraper siteScraperRun of
          Nothing -> acc
          Just scrapedRecipe -> scrapedRecipe:acc
        _ -> acc
  case flip lookup siteScrapers =<< domainMay of
    Just SiteScraper {..} -> maybe (throwError "Failed to scrape known URL") pure $ runScraper siteScraperRun
    Nothing -> maybe (throwError "Failed to scrape URL from defaults") pure
      . lastMay
      . sortOn (length . scrapedRecipeIngredients)
      . foldr go mempty
      $ allSiteScrapers
