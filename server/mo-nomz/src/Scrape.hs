module Scrape where

import ClassyPrelude

import Control.Monad (fail)
import Control.Monad.Except (MonadError, throwError)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Text (split, strip)
import Network.URI (URI)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.CaseInsensitive as CI
import qualified Text.HTML.Scalpel as Scalpel

import Scrub (quantityAliasTable)
import Types (IngredientName(..), Quantity(..), RawIngredient(..), RawQuantity(..), RawUnit(..))

scrapeUrl :: (MonadIO m, MonadError Text m) => URI -> m Text
scrapeUrl uri = do
  liftIO (Scalpel.scrapeURL (show uri) (Scalpel.chroot "body" (Scalpel.texts Scalpel.anySelector))) >>= \case
    Nothing -> throwError "Failed to scrape URL"
    Just xs -> pure . unlines . ordNub . filter (not . null) . map strip . lines . unlines $ xs

quantityP :: Atto.Parser RawQuantity
quantityP = quantityPure <|> quantityFrac <|> quantityDec <|> quantityWord <|> quantityMissing
  where
    isQuantityC c = isDigit c || isSpace c || c == '/' || c == '.'
    parsePure str = maybe (fail $ unpack str <> " is not a quantity") (pure . Quantity) . readMay . unpack . filter (not . isSpace) $ str
    parseFrac str = case split ((==) '/') $ filter (not . isSpace) str of
      [x, y] -> maybe (fail $ unpack str <> " is not a quantity") (pure . Quantity) $
        (/) <$> readMay x <*> readMay y
      _ -> fail $ unpack str <> " is not a quantity"
    parseDec str = case split ((==) '.') $ filter (not . isSpace) str of
      [x, y] -> maybe (fail $ unpack str <> " is not a quantity") (pure . Quantity) $ do
        x' <- fromInteger <$> readMay x
        y' <- fromInteger <$> readMay y
        pure $ x' + (y' / (fromIntegral $ 10 * length y))
      _ -> fail $ unpack str <> " is not a quantity"
    quantityPure = RawQuantityPure <$> (parsePure =<< Atto.takeWhile1 isQuantityC)
    quantityFrac = RawQuantityPure <$> (parseFrac =<< Atto.takeWhile1 isQuantityC)
    quantityDec = RawQuantityPure <$> (parseDec =<< Atto.takeWhile1 isQuantityC)
    quantityWord = RawQuantityWord . CI.mk <$> ((\str -> if CI.mk str `elem` keys quantityAliasTable then pure str else fail $ unpack str <> " is not a quantity") =<< spaced (Atto.takeWhile1 isAlpha))
    quantityMissing = pure RawQuantityMissing

spaced :: Atto.Parser a -> Atto.Parser a
spaced p = optional (void Atto.space) *> (p <* optional (void Atto.space))

unitP :: Atto.Parser RawUnit
unitP = RawUnit . CI.mk <$> spaced (Atto.takeWhile1 isAlpha)

nameP :: Atto.Parser IngredientName
nameP = IngredientName . CI.mk <$> spaced (Atto.takeWhile1 (not . (==) '\n'))

ingredientP :: Atto.Parser RawIngredient
ingredientP = mk <$> ((,,) <$> quantityP <*> unitP <*> nameP)
  where
    mk (q, u, n) = RawIngredient n q u

ingredientsP :: Atto.Parser [RawIngredient]
ingredientsP = Atto.many' ingredientP

parseIngredients :: (MonadError Text m) => Text -> m [RawIngredient]
parseIngredients val = do
  let startsWithIngredients x = "ingredient" `isPrefixOf` toLower x
      accumulatePosibilities acc next = case startsWithIngredients next of
        False -> (next:) <$> acc
        True -> []:acc
      possibilities = map (unlines . reverse) . foldl' accumulatePosibilities mempty . dropWhile (not . startsWithIngredients) . lines $ val
  maybe (throwError "Failed to parse ingredients") pure . headMay . sortOn length . rights . map (Atto.parseOnly ingredientsP) $ possibilities
