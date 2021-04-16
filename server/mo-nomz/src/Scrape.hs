module Scrape where

import ClassyPrelude

import Control.Monad (fail)
import Control.Monad.Except (MonadError, throwError)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Text (split, strip)
import Network.URI (URI)
import Text.HTML.Scalpel ((//), (@:))
import qualified Data.Attoparsec.Text as Atto
import qualified Data.CaseInsensitive as CI
import qualified Text.HTML.Scalpel as Scalpel

import Scrub (quantityAliasTable)
import Types (IngredientName(..), Quantity(..), RawIngredient(..), RawQuantity(..), RawUnit(..))

data ScrapedRecipe = ScrapedRecipe
  { scrapedRecipeTitle    :: Text
  , scrapedRecipeContents :: Text
  }

scrapeUrl :: (MonadIO m, MonadError Text m) => URI -> m ScrapedRecipe
scrapeUrl uri = do
  let containsIngredientClass = Scalpel.match $ \attributeKey attributeValue -> case attributeKey of
        "class" -> "ingredient" `isInfixOf` toLower attributeValue
        _ -> False
      title = strip <$> Scalpel.text "title"
      contents = Scalpel.chroots (Scalpel.AnyTag @: [containsIngredientClass] // "li") (Scalpel.texts Scalpel.anySelector)
  liftIO (Scalpel.scrapeURL (show uri) ((,) <$> title <*> contents)) >>= \case
    Nothing -> throwError "Failed to scrape URL"
    Just (x, xs) -> pure . ScrapedRecipe x . (<> "\n") . unlines . ordNub . filter (not . null) . map strip . lines . unlines . map unlines $ xs

quantityP :: Atto.Parser RawQuantity
quantityP = quantityExpression <|> quantityWord <|> quantityMissing
  where
    isQuantityC c = isDigit c || isSpace c || elem c ['/', '.', '-', '¼', '½', '¾', '⅓', '⅔']
    quantityParser p = p =<< Atto.takeWhile isQuantityC
    strictQuantityParser p = p . strip =<< Atto.takeWhile1 isQuantityC

    quantitySingle str = maybe (fail $ unpack str <> " is not a single quantity") (pure . Quantity) . readMay . unpack . filter (not . isSpace) $ str
    quantityUnicode = \case
      "¼" -> pure $ Quantity 0.25
      "½" -> pure $ Quantity 0.5
      "¾" -> pure $ Quantity 0.75
      "⅓" -> pure $ Quantity $ 1 / 3
      "⅔" -> pure $ Quantity $ 2 / 3
      str -> fail $ unpack str <> " is not a unicode quantity"
    quantityDecimal str = case split ((==) '.') str of
      [x, y] -> maybe (fail $ unpack str <> " is not a decimal quantity") (pure . Quantity) $ do
        x' <- fromInteger <$> readMay x
        y' <- fromInteger <$> readMay y
        pure $ x' + (y' / (fromIntegral $ 10 * length y))
      _ -> fail $ unpack str <> " is not a decimal quantity"
    quantityFraction str = case split ((==) '/') str of
      [x, y] -> maybe (fail $ unpack str <> " is not a fractional quantity") (pure . Quantity) $
        (/) <$> readMay x <*> readMay y
      _ -> fail $ unpack str <> " is not a fractional quantity"

    quantityImproper = quantityParser $ \str -> case filter (not . null) . mconcat . map (split isSpace) . split ((==) '-') $ str of
      [x, y] -> (+) <$> quantitySimple x <*> quantitySimple y
      _ -> fail $ unpack str <> " is not an improper quantity"

    quantitySimple str =
      quantitySingle str
        <|> quantityUnicode str
        <|> quantityDecimal str
        <|> quantityFraction str

    quantityExpression = RawQuantityPure <$> (strictQuantityParser quantitySimple <|> quantityImproper)
    quantityWord = RawQuantityWord . CI.mk <$> ((\str -> if CI.mk str `elem` keys quantityAliasTable then pure str else fail $ unpack str <> " is not a quantity") =<< spaced (Atto.takeWhile1 isAlpha))
    quantityMissing = quantityParser $ \str -> case null str of
      True -> pure RawQuantityMissing
      False -> fail $ unpack str <> " is a quantity, but thought it was missing"

spaced :: Atto.Parser a -> Atto.Parser a
spaced p = optional (void Atto.space) *> (p <* optional (void Atto.space))

unitP :: Atto.Parser RawUnit
unitP = unitWord <|> unitMissing
  where
    unitWord = RawUnitWord . CI.mk <$> spaced (Atto.takeWhile1 isAlpha)
    unitMissing = do
      str <- spaced (Atto.takeWhile isAlpha)
      case null str of
        True -> pure RawUnitMissing
        False -> fail $ unpack str <> " is a unit, but thought it was missing"

nameP :: Atto.Parser IngredientName
nameP = IngredientName . CI.mk <$> spaced (Atto.takeWhile1 (not . (==) '\n'))

ingredientP :: Atto.Parser RawIngredient
ingredientP = mk <$> ((,,) <$> quantityP <*> unitP <*> nameP)
  where
    mk (q, u, n) = RawIngredient n q u

ingredientsP :: Atto.Parser [RawIngredient]
ingredientsP = Atto.many' ingredientP

parseIngredients :: (MonadError Text m) => Text -> m [RawIngredient]
parseIngredients = either (const $ throwError "Failed to parse ingredients") pure . Atto.parseOnly ingredientsP
