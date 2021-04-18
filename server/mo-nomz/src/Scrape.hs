module Scrape where

import ClassyPrelude

import Control.Monad (fail)
import Control.Monad.Except (MonadError, throwError)
import Data.CaseInsensitive (CI)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Text (split, strip)
import Network.URI (URI)
import Text.HTML.Scalpel ((//), (@:))
import qualified Data.Attoparsec.Text as Atto
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Text.HTML.Scalpel as Scalpel

import Types
  ( Ingredient(..), IngredientName(..), Quantity(..), RawIngredient(..), RawQuantity(..)
  , RawUnit(..), RecipeName(..), Unit(..), Ingredient, box, cup, ounce, pinch, pound, splash
  , sprinkle, tablespoon, teaspoon, whole
  )

data ScrapedRecipe = ScrapedRecipe
  { scrapedRecipeTitle    :: RecipeName
  , scrapedRecipeContents :: Text
  } deriving (Eq, Show)

scrapeUrl :: (MonadIO m, MonadError Text m) => URI -> m ScrapedRecipe
scrapeUrl uri = do
  let containsIngredientClass = Scalpel.match $ \attributeKey attributeValue -> case attributeKey of
        "class" -> "ingredient" `isInfixOf` toLower attributeValue
        _ -> False
      title = RecipeName . strip <$> Scalpel.text "title"
      contents = Scalpel.chroots (Scalpel.AnyTag @: [containsIngredientClass] // "li") (Scalpel.texts Scalpel.anySelector)
  liftIO (Scalpel.scrapeURL (show uri) ((,) <$> title <*> contents)) >>= \case
    Nothing -> throwError "Failed to scrape URL"
    Just (x, xs) -> pure . ScrapedRecipe x . unlines . map unlines $ xs

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
  [ ("half dozen", Quantity 6)
  , ("dozen", Quantity 12)
  , ("quarter", Quantity 0.25)
  , ("third", Quantity $ 1 / 3)
  , ("half", Quantity 0.5)
  , ("one", Quantity 1)
  , ("two", Quantity 2)
  , ("three", Quantity 3)
  , ("four", Quantity 4)
  , ("five", Quantity 5)
  , ("six", Quantity 6)
  , ("seven", Quantity 7)
  , ("eight", Quantity 8)
  , ("nine", Quantity 9)
  , ("ten", Quantity 10)
  , ("eleven", Quantity 11)
  , ("twelve", Quantity 12)
  ]

scrubUnit :: RawUnit -> Unit
scrubUnit = \case
  RawUnitWord x -> findWithDefault (Unit x) x unitAliasTable
  RawUnitMissing -> whole

scrubQuantity :: RawQuantity -> Quantity
scrubQuantity = \case
  RawQuantityPure q -> q
  RawQuantityWord w -> findWithDefault 1 w quantityAliasTable
  RawQuantityMissing -> 1

scrubIngredient :: RawIngredient -> Ingredient
scrubIngredient RawIngredient {..} = Ingredient
  { ingredientName = rawIngredientName
  , ingredientQuantity = scrubQuantity rawIngredientQuantity
  , ingredientUnit = scrubUnit rawIngredientUnit
  , ingredientActive = True
  }
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
unitP = unitWord <|> pure RawUnitMissing
  where
    unitWord = do
      unit <- CI.mk <$> spaced (Atto.takeWhile1 isAlpha)
      case unit `elem` keys unitAliasTable of
        True -> pure $ RawUnitWord unit
        False -> fail "no unit found"

nameP :: Atto.Parser IngredientName
nameP = IngredientName . CI.mk <$> spaced (Atto.takeWhile1 (not . (==) '\n'))

ingredientP :: Atto.Parser RawIngredient
ingredientP = mk <$> ((,,) <$> quantityP <*> unitP <*> nameP)
  where
    mk (q, u, n) = RawIngredient n q u

ingredientsP :: Atto.Parser [RawIngredient]
ingredientsP = Atto.many' ingredientP

deduplicateIngredients :: [RawIngredient] -> [RawIngredient]
deduplicateIngredients = catMaybes . Map.elems . map (headMay . sort) . foldr (\raw@RawIngredient {..} -> insertWith (<>) rawIngredientName [raw]) mempty

parseIngredients :: (MonadError Text m) => Text -> m [Ingredient]
parseIngredients =
  either (const $ throwError "Failed to parse ingredients") (pure . map scrubIngredient . deduplicateIngredients)
    . Atto.parseOnly ingredientsP
    . (<> "\n")
    . unlines
    . ordNub
    . filter (not . null)
    . map strip
    . lines
