module Scrape (module Scrape, ScrapedRecipe(..)) where

import ClassyPrelude

import Control.Monad (fail)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Logger (MonadLogger, logError, runStdoutLoggingT)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (replace)
import Network.URI (URI, parseURI, uriAuthority, uriRegName)
import qualified Text.HTML.Scalpel as Scalpel

import Conversion (mkReadableIngredient)
import Foundation (HasManager, createManager, manager)
import Parser (parseIngredients, parseSteps)
import Scraper.Internal.Site
  ( allIngredientScrapers, allStepScrapers, ingredientScrapers, stepScrapers
  )
import Scraper.Internal.Types
  ( IngredientScraper(..), ScrapedRecipe(..), SiteName(..), StepScraper(..), title
  )
import Types (OrderedIngredient(..), RecipeName(..), Step(..))

scrapeUrl :: (HasManager r, MonadIO m, MonadError Text m, MonadLogger m, MonadReader r m) => URI -> m ScrapedRecipe
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
        Just True -> runScraper runIngredientParser ingredientScraperRun
        _ -> Nothing
      goStep StepScraper {..} = case Scalpel.scrape stepScraperTest tags of
        Just True -> runScraper runStepParser stepScraperRun
        _ -> Nothing
  ingredients <- case flip lookup ingredientScrapers =<< domainMay of
    Just IngredientScraper {..} -> maybe (throwError "Failed to scrape known URL") pure $ runScraper runIngredientParser ingredientScraperRun
    Nothing -> maybe (throwError "Failed to scrape URL from defaults") pure
      . lastMay
      . sortOn length
      . mapMaybe goIngredient
      $ allIngredientScrapers
  steps <- case flip lookup stepScrapers =<< domainMay of
    Just StepScraper {..} -> pure . fromMaybe [] . runScraper runStepParser $ stepScraperRun
    Nothing -> pure
      . fromMaybe []
      . lastMay
      . sortOn length
      . mapMaybe goStep
      $ allStepScrapers
  when (null steps) $ $logError $ "No steps scraped for " <> tshow uri
  pure $ ScrapedRecipe name ingredients steps

unsafeScrapeUrl :: String -> IO ()
unsafeScrapeUrl url = do
  uri <- maybe (fail "Invalid link") pure $ parseURI url
  man <- createManager
  recipe <- either (fail . unpack) pure
    =<< runStdoutLoggingT (runExceptT (runReaderT (scrapeUrl uri) man))
  putStrLn . toStrict . decodeUtf8 . encodePretty
    . map (mkReadableIngredient . uncurry OrderedIngredient)
    . flip zip [1..]
    . scrapedRecipeIngredients
    $ recipe
  putStrLn . toStrict . decodeUtf8 . encodePretty . map unStep . scrapedRecipeSteps $ recipe
