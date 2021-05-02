module Parser where

import ClassyPrelude

import Control.Arrow (left)
import Control.Monad (fail)
import Data.CaseInsensitive (CI)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Text (replace, split, strip)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.CaseInsensitive as CI

import Scraper.Internal.Types (UnparsedIngredient(..))
import Types
  ( Ingredient(..), IngredientName(..), Quantity(..), RawIngredient(..), RawQuantity(..)
  , RawUnit(..), Unit(..), Ingredient, box, cup, ounce, pinch, pound, splash, sprinkle, tablespoon
  , teaspoon, whole
  )

unitAliasTable :: Map (CI Text) Unit
unitAliasTable = mapFromList
  [ ("ounce", ounce)
  , ("ounces", ounce)
  , ("oz", ounce)
  , ("cup", cup)
  , ("cups", cup)
  , ("tablespoon", tablespoon)
  , ("tablespoons", tablespoon)
  , ("tbsp", tablespoon)
  , ("teaspoon", teaspoon)
  , ("teaspoons", teaspoon)
  , ("tsp", teaspoon)
  , ("pinch", pinch)
  , ("pinches", pinch)
  , ("box", box)
  , ("boxes", box)
  , ("pound", pound)
  , ("pounds", pound)
  , ("splash", splash)
  , ("splashes", splash)
  , ("sprinkle", sprinkle)
  , ("sprinkles", sprinkle)
  , ("whole", whole)
  ]

quantityAliasTable :: Map (CI Text) Quantity
quantityAliasTable = mapFromList
  [ ("half dozen", 6)
  , ("dozen", 12)
  , ("quarter", 0.25)
  , ("third", 1 / 3)
  , ("half", 0.5)
  , ("one", 1)
  , ("two", 2)
  , ("three", 3)
  , ("four", 4)
  , ("five", 5)
  , ("six", 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine", 9)
  , ("ten", 10)
  , ("eleven", 11)
  , ("twelve", 12)
  ]

scrubUnit :: RawUnit -> Unit
scrubUnit = \case
  RawUnit x -> findWithDefault (Unit x) x unitAliasTable
  RawUnitMissing -> UnitMissing

scrubQuantity :: RawQuantity -> Quantity
scrubQuantity = \case
  RawQuantity q -> Quantity q
  RawQuantityWord w -> findWithDefault QuantityMissing w quantityAliasTable
  RawQuantityMissing -> QuantityMissing

scrubIngredient :: RawIngredient -> Ingredient
scrubIngredient RawIngredient {..} = Ingredient
  { ingredientName = rawIngredientName
  , ingredientQuantity = scrubQuantity rawIngredientQuantity
  , ingredientUnit = scrubUnit rawIngredientUnit
  }

quantityP :: Atto.Parser RawQuantity
quantityP = quantityExpression <|> quantityWord <|> quantityMissing
  where
    isIgnoredC c = elem c ['Â']
    isQuantityC c = isDigit c || isSpace c || elem c ['/', '.', '-', '⁄', '¼', '½', '¾', '⅓', '⅔'] || isIgnoredC c
    quantityParser p = p . filter (not . isIgnoredC) =<< Atto.takeWhile isQuantityC
    strictQuantityParser p = p . strip . filter (not . isIgnoredC) =<< Atto.takeWhile1 isQuantityC

    quantitySingle str = maybe (fail $ unpack str <> " is not a single quantity") pure . readMay . unpack . filter (not . isSpace) $ str
    quantityUnicode = \case
      "¼" -> pure 0.25
      "½" -> pure 0.5
      "¾" -> pure 0.75
      "⅓" -> pure $ 1 / 3
      "⅔" -> pure $ 2 / 3
      str -> fail $ unpack str <> " is not a unicode quantity"
    quantityDecimal str = case split ((==) '.') str of
      [x, y] -> maybe (fail $ unpack str <> " is not a decimal quantity") pure $ do
        x' <- fromInteger <$> readMay x
        y' <- fromInteger <$> readMay y
        pure $ x' + (y' / (fromIntegral $ 10 * length y))
      _ -> fail $ unpack str <> " is not a decimal quantity"
    quantityFraction str = case split ((==) '/') $ replace "⁄" "/" str of
      [x, y] -> maybe (fail $ unpack str <> " is not a fractional quantity") pure $
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

    quantityExpression = RawQuantity <$> (strictQuantityParser quantitySimple <|> quantityImproper)
    quantityWord = RawQuantityWord . CI.mk <$> ((\str -> if CI.mk str `elem` keys quantityAliasTable then pure str else fail $ unpack str <> " is not a quantity") =<< spaced (Atto.takeWhile1 isAlpha))
    quantityMissing = quantityParser $ \str -> case null str of
      True -> pure RawQuantityMissing
      False -> fail $ unpack str <> " is a quantity, but thought it was missing"

spaced :: Atto.Parser a -> Atto.Parser a
spaced p = optional (void Atto.space) *> (p <* optional (void Atto.space))

unitP :: Atto.Parser RawUnit
unitP = unitWord <|> pure RawUnitMissing
  where
    isIgnoredC c = elem c ['.']
    isUnitC c = isAlpha c || isIgnoredC c
    unitWord = do
      unit <- CI.mk . filter (not . isIgnoredC) <$> spaced (Atto.takeWhile1 isUnitC)
      case unit `elem` keys unitAliasTable of
        True -> pure $ RawUnit unit
        False -> fail "No unit found"

nameP :: Atto.Parser IngredientName
nameP = IngredientName . CI.mk . strip . unwords . filter (not . null) . map strip . words <$> Atto.takeText

ingredientP :: Atto.Parser RawIngredient
ingredientP = mk <$> ((,,) <$> quantityP <*> unitP <*> nameP)
  where
    mk (q, u, n) = RawIngredient n q u

sanitize :: Text -> Text
sanitize = replace "\194" " " . filter (not . isIgnoredC)
  where
    isIgnoredC c = elem c ['▢']

runParser :: Atto.Parser a -> Text -> Either String a
runParser parser x = Atto.parseOnly parser (strip (sanitize x))

parseIngredients :: [UnparsedIngredient] -> Either Text [Ingredient]
parseIngredients xs = left (const "Failed to parse ingredients") . map (ordNub . map scrubIngredient . catMaybes) . for xs $ \case
  UnparsedIngredientRaw raw | null raw -> pure Nothing
  UnparsedIngredientRaw raw -> Just <$> runParser ingredientP raw

parseRawIngredients :: Text -> Either Text [Ingredient]
parseRawIngredients content = do
  either (const $ Left "Failed to parse ingredients") (pure . map scrubIngredient) $
    traverse (runParser ingredientP) $ filter (not . null) $ lines content
