module ScrapeSpec where

import ClassyPrelude

import Control.Monad.Except (runExcept)
import Data.FileEmbed (embedFile)
import System.FilePath.TH (fileRelativeToAbsoluteStr)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import qualified Data.Attoparsec.Text as Atto

import Types (IngredientName(..), RawIngredient(..), RawQuantity(..), RawUnit(..))

import Scrape

parseStrict :: (Eq a, Show a) => a -> Atto.Parser a -> Text -> Expectation
parseStrict expected parser input = Atto.parseOnly parser input `shouldBe` Right expected

spec :: Spec
spec = describe "Scrape" $ do
  describe "Examples" $ do
    it "can parse a unit" $ parseStrict (RawUnit "ounces") unitP " ounces "
    it "can parse a fraction" $ parseStrict (RawQuantityPure $ 1 / 3) quantityP "\n1\n/\n3\n"
    it "can parse a decimal" $ parseStrict (RawQuantityPure 0.25) quantityP "0.25"
    it "can parse a word" $ parseStrict (RawQuantityWord "half") quantityP "\nhalf\n"
    it "can parse an ingredient name" $ parseStrict (IngredientName "chicken") nameP " chicken"
    it "can parse \"whole chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") RawQuantityMissing (RawUnit "whole")) ingredientP "whole chicken"
    it "can parse \"one whole chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") (RawQuantityWord "one") (RawUnit "whole")) ingredientP "one whole chicken"
    it "can parse \"1/4 cup broth\"" $ parseStrict (RawIngredient (IngredientName "broth") (RawQuantityPure $ 0.25) (RawUnit "cup")) ingredientP "1\n/\n4\ncup\nbroth"

  describe "Golden" $ do
    it "can parse allrecipes" $
      let expected = []
      in runExcept (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/chicken-pot-pie-allrecipes.txt")) `shouldBe` Right expected

    it "can parse pillsbury" $
      let expected = []
      in runExcept (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/chicken-pot-pie-pillsbury.txt")) `shouldBe` Right expected

    it "can parse taste of home" $
      let expected = []
      in runExcept (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/chicken-pot-pie-tasteofhome.txt")) `shouldBe` Right expected
