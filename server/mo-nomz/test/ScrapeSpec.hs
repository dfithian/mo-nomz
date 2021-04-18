module ScrapeSpec where

import ClassyPrelude

import Control.Monad.Except (runExcept)
import Data.Either (fromRight)
import Data.FileEmbed (embedFile)
import System.FilePath.TH (fileRelativeToAbsoluteStr)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldMatchList)
import qualified Data.Attoparsec.Text as Atto

import ParsedIngredients
  ( allRecipesIngredients, foodNetworkIngredients, pillsburyIngredients, rachelMansfieldIngredients
  , tasteOfHomeIngredients
  )
import Types (IngredientName(..), RawIngredient(..), RawQuantity(..), RawUnit(..))

import Scrape

parseStrict :: (Eq a, Show a) => a -> Atto.Parser a -> Text -> Expectation
parseStrict expected parser input = Atto.parseOnly parser input `shouldBe` Right expected

spec :: Spec
spec = describe "Scrape" $ do
  describe "Examples" $ do
    it "can parse a unit" $ parseStrict (RawUnitWord "ounces") unitP " ounces "
    it "can parse a fraction" $ parseStrict (RawQuantityPure $ 1 / 3) quantityP "1/3"
    it "can parse an improper fraction" $ parseStrict (RawQuantityPure 1.5) quantityP "1-1/2"
    it "can parse an improper fraction with spaces" $ parseStrict (RawQuantityPure 1.5) quantityP "1 1/2"
    it "can parse a decimal" $ parseStrict (RawQuantityPure 0.25) quantityP "0.25"
    it "can parse a word" $ parseStrict (RawQuantityWord "half") quantityP "\nhalf\n"
    it "can parse an ingredient name" $ parseStrict (IngredientName "chicken") nameP " chicken"
    it "can parse \"chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") RawQuantityMissing RawUnitMissing) ingredientP "chicken"
    it "can parse \"one chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") (RawQuantityWord "one") RawUnitMissing) ingredientP "one chicken"
    it "can parse \"whole chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") RawQuantityMissing (RawUnitWord "whole")) ingredientP "whole chicken"
    it "can parse \"one whole chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") (RawQuantityWord "one") (RawUnitWord "whole")) ingredientP "one whole chicken"
    it "can parse \"1/4 cup broth\"" $ parseStrict (RawIngredient (IngredientName "broth") (RawQuantityPure 0.25) (RawUnitWord "cup")) ingredientP "1/4\ncup\nbroth"

  describe "Golden" $ do
    it "can parse allrecipes" $
      fromRight [] (runExcept (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/chicken-pot-pie-allrecipes.txt"))) `shouldMatchList` allRecipesIngredients

    it "can parse pillsbury" $
      fromRight [] (runExcept (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/chicken-pot-pie-pillsbury.txt"))) `shouldMatchList` pillsburyIngredients

    it "can parse taste of home" $
      fromRight [] (runExcept (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/chicken-pot-pie-tasteofhome.txt"))) `shouldMatchList` tasteOfHomeIngredients

    it "can parse rachel mansfield" $
      fromRight [] (runExcept (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/banana-bread-rachelmansfield.txt"))) `shouldMatchList` rachelMansfieldIngredients

    it "can parse food network" $
      fromRight [] (runExcept (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/roast-chicken-food-network.txt"))) `shouldMatchList` foodNetworkIngredients
