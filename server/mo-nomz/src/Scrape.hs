module Scrape where

import Prelude

import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logError, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (toStrict)
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, pack, replace, unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.URI (URI, parseURI, uriAuthority, uriRegName)
import qualified Data.HashMap.Strict as HashMap
import qualified Text.HTML.Scalpel as Scalpel

import Conversion (mkReadableIngredient)
import Foundation (HasManager, createManager, manager)
import Parser (parseIngredients, parseSteps)
import Scraper.Site (allIngredientScrapers, allStepScrapers, ingredientScrapers, stepScrapers)
import Scraper.Types
  ( IngredientScraper(..), ScrapeInfo(..), ScrapedInfo(..), ScrapedRecipe(..), SiteName(..)
  , StepScraper(..), title
  )
import Types (OrderedIngredient(..), RecipeName(..), Step(..), lastMay, tshow)

scrapeUrl :: (HasManager r, MonadIO m, MonadError Text m, MonadLogger m, MonadReader r m) => URI -> m (ScrapedRecipe, ScrapedInfo)
scrapeUrl uri = do
  cfg <- Scalpel.Config Scalpel.defaultDecoder . Just <$> asks manager
  tags <- liftIO $ Scalpel.fetchTagsWithConfig cfg (show uri)
  let domainMay = SiteName . replace "www." "" . pack . uriRegName <$> uriAuthority uri
      name = fromMaybe (RecipeName "Untitled") $ Scalpel.scrape title tags
      runIngredientParser rawIngredients =
        parseIngredients rawIngredients >>= \case
          [] -> Left "No ingredients found"
          xs -> Right xs
      runStepParser rawSteps =
        parseSteps rawSteps >>= \case
          [] -> Left "No steps found"
          xs -> Right xs

      runScraper :: forall a b. ([a] -> Either Text [b]) -> Scalpel.Scraper Text [a] -> Maybe [b]
      runScraper parser scraper = either (const Nothing) Just . parser =<< Scalpel.scrape scraper tags

      goIngredient IngredientScraper {..} = case Scalpel.scrape ingredientScraperTest tags of
        Just True -> (,ingredientScraperInfo) <$> runScraper runIngredientParser ingredientScraperRun
        _ -> Nothing
      goStep StepScraper {..} = case Scalpel.scrape stepScraperTest tags of
        Just True -> (,stepScraperInfo) <$> runScraper runStepParser stepScraperRun
        _ -> Nothing
  (ingredients, ingredientInfo) <- case flip HashMap.lookup ingredientScrapers =<< domainMay of
    Just IngredientScraper {..} -> maybe (throwError "Failed to scrape known URL") (pure . (,ingredientScraperInfo)) $ runScraper runIngredientParser ingredientScraperRun
    Nothing -> maybe (throwError "Failed to scrape URL from defaults") pure
      . lastMay
      . sortOn (length . fst)
      . mapMaybe goIngredient
      $ allIngredientScrapers
  stepsMay <- case flip HashMap.lookup stepScrapers =<< domainMay of
    Just StepScraper {..} -> pure . fmap (,stepScraperInfo) . runScraper runStepParser $ stepScraperRun
    Nothing -> pure
      . lastMay
      . sortOn (length . fst)
      . mapMaybe goStep
      $ allStepScrapers
  case stepsMay of
    Just (steps, stepInfo) | not (null steps) -> pure (ScrapedRecipe name ingredients steps, ScrapedInfoIngredientStep ingredientInfo stepInfo)
    _ -> do
      $logError $ "No steps scraped for " <> tshow uri
      pure (ScrapedRecipe name ingredients [], ScrapedInfoIngredient ingredientInfo)

isInvalidScraper :: ScrapedInfo -> Bool
isInvalidScraper scrapeInfo =
  let matchesIngredientVersion info =
        (==) (Just (scrapeInfoVersion info))
          . fmap (scrapeInfoVersion . ingredientScraperInfo)
          . find ((==) (scrapeInfoName info) . scrapeInfoName . ingredientScraperInfo)
          $ allIngredientScrapers
      matchesStepVersion info =
        (==) (Just (scrapeInfoVersion info))
          . fmap (scrapeInfoVersion . stepScraperInfo)
          . find ((==) (scrapeInfoName info) . scrapeInfoName . stepScraperInfo)
          $ allStepScrapers
  in case scrapeInfo of
    ScrapedInfoIngredient ingredient -> not (matchesIngredientVersion ingredient)
    ScrapedInfoIngredientStep ingredient step -> not (matchesIngredientVersion ingredient) || not (matchesStepVersion step)

unsafeScrapeUrl :: String -> IO ()
unsafeScrapeUrl url = do
  uri <- maybe (fail "Invalid link") pure $ parseURI url
  man <- createManager
  recipe <- either (fail . unpack) (pure . fst)
    =<< runStdoutLoggingT (runExceptT (runReaderT (scrapeUrl uri) man))
  putStrLn . unpack . decodeUtf8 . toStrict . encodePretty
    . fmap (mkReadableIngredient . uncurry OrderedIngredient)
    . flip zip [1..]
    . scrapedRecipeIngredients
    $ recipe
  putStrLn . unpack . decodeUtf8 . toStrict . encodePretty . fmap unStep . scrapedRecipeSteps $ recipe
