module Scrape (module Scrape, ScrapedRecipe(..)) where

import ClassyPrelude

import Control.Monad (fail)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (replace)
import Network.URI (URI, parseURI, uriAuthority, uriRegName)
import qualified Text.HTML.Scalpel as Scalpel

import Conversion (mkReadableIngredient)
import Foundation (HasManager, createManager, manager)
import Parser (parseIngredients, parseSteps)
import Scraper.Internal.Site (allSiteScrapers, siteScrapers)
import Scraper.Internal.Types (ScrapedRecipe(..), SiteName(..), SiteScraper(..), title)
import Types (OrderedIngredient(..), RecipeName(..))

scrapeUrl :: (HasManager r, MonadIO m, MonadError Text m, MonadReader r m) => URI -> m ScrapedRecipe
scrapeUrl uri = do
  cfg <- Scalpel.Config Scalpel.defaultDecoder . Just <$> asks manager
  tags <- liftIO $ Scalpel.fetchTagsWithConfig cfg (show uri)
  let domainMay = SiteName . replace "www." "" . pack . uriRegName <$> uriAuthority uri
      name = fromMaybe (RecipeName "Untitled") $ Scalpel.scrape title tags
      runParser rawIngredients rawSteps = do
        ingredients <- parseIngredients rawIngredients
        steps <- parseSteps rawSteps
        case null ingredients of
          True -> Left "No ingredients found"
          False -> Right $ ScrapedRecipe name ingredients steps
      runScraper scraper = either (const Nothing) Just . uncurry runParser =<< Scalpel.scrape scraper tags
      go SiteScraper {..} acc = case Scalpel.scrape siteScraperTest tags of
        Just True -> case runScraper siteScraperRun of
          Nothing -> acc
          Just scrapedRecipe -> scrapedRecipe:acc
        _ -> acc
  case flip lookup siteScrapers =<< domainMay of
    Just SiteScraper {..} -> maybe (throwError "Failed to scrape known URL") pure $ runScraper siteScraperRun
    Nothing -> maybe (throwError "Failed to scrape URL from defaults") pure
      . lastMay
      . sortOn (length . scrapedRecipeIngredients &&& length . scrapedRecipeSteps)
      . foldr go mempty
      $ allSiteScrapers

unsafeScrapeUrl :: String -> IO ()
unsafeScrapeUrl url = do
  uri <- maybe (fail "Invalid link") pure $ parseURI url
  man <- createManager
  ingredients <- either (fail . unpack) (pure . scrapedRecipeIngredients)
    =<< runExceptT (runReaderT (scrapeUrl uri) man)
  putStrLn . toStrict . decodeUtf8 . encodePretty
    . map (mkReadableIngredient . uncurry OrderedIngredient)
    . flip zip [1..]
    $ ingredients
