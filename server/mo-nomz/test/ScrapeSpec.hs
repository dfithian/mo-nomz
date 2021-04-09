module ScrapeSpec where

import ClassyPrelude

import Control.Monad.Except (runExcept)
import Data.FileEmbed (embedFile)
import System.FilePath.TH (fileRelativeToAbsoluteStr)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.CaseInsensitive as CI

import Types (IngredientName(..), Quantity(..), RawIngredient(..), RawQuantity(..), RawUnit(..))

import Scrape

parseStrict :: (Eq a, Show a) => a -> Atto.Parser a -> Text -> Expectation
parseStrict expected parser input = Atto.parseOnly parser input `shouldBe` Right expected

rawPure :: Double -> Text -> Text -> RawIngredient
rawPure q u i = RawIngredient
  { rawIngredientName = IngredientName $ CI.mk i
  , rawIngredientQuantity = RawQuantityPure $ Quantity q
  , rawIngredientUnit = RawUnitWord $ CI.mk u
  }

rawWord :: Text -> Text -> Text -> RawIngredient
rawWord q u i = RawIngredient
  { rawIngredientName = IngredientName $ CI.mk i
  , rawIngredientQuantity = RawQuantityWord $ CI.mk q
  , rawIngredientUnit = RawUnitWord $ CI.mk u
  }

rawMissing :: Text -> Text -> RawIngredient
rawMissing u i = RawIngredient
  { rawIngredientName = IngredientName $ CI.mk i
  , rawIngredientQuantity = RawQuantityMissing
  , rawIngredientUnit = RawUnitWord $ CI.mk u
  }

rawPureMissingUnit :: Double -> Text -> RawIngredient
rawPureMissingUnit q i = RawIngredient
  { rawIngredientName = IngredientName $ CI.mk i
  , rawIngredientQuantity = RawQuantityPure $ Quantity q
  , rawIngredientUnit = RawUnitMissing
  }

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
    it "can parse \"whole chicken\"" $ parseStrict (rawMissing "whole" "chicken") ingredientP "whole chicken"
    it "can parse \"one whole chicken\"" $ parseStrict (rawWord "one" "whole" "chicken") ingredientP "one whole chicken"
    it "can parse \"1/4 cup broth\"" $ parseStrict (rawPure 0.25 "cup" "broth") ingredientP "1/4\ncup\nbroth"

  describe "Golden" $ do
    it "can parse allrecipes" $
      let expected =
            [ rawPure 1 "pound" "skinless, boneless chicken breast halves - cubed"
            , rawPure 1 "cup" "sliced carrots"
            , rawPure 1 "cup" "frozen green peas"
            , rawPure 0.5 "cup" "sliced celery"
            , rawPure (1 / 3) "cup" "butter"
            , rawPure (1 / 3) "cup" "chopped onion"
            , rawPure (1 / 3) "cup" "all-purpose flour"
            , rawPure 0.5 "teaspoon" "salt"
            , rawPure 0.25 "teaspoon" "black pepper"
            , rawPure 0.25 "teaspoon" "celery seed"
            , rawPure 1.75 "cups" "chicken broth"
            , rawPure (2 / 3) "cup" "milk"
            , rawPureMissingUnit 2 "(9 inch)  unbaked pie crusts"
            ]
      in runExcept (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/chicken-pot-pie-allrecipes.txt")) `shouldBe` Right expected

    it "can parse pillsbury" $
      let expected =
            [ rawPure 1 "box" "Pillsbury\\8482 refrigerated pie crusts, softened as directed on box"
            , rawPure (1 / 3) "cup" "butter or margarine"
            , rawMissing "cup" "chopped onion"
            , rawMissing "cup" "all-purpose flour"
            , rawPure 0.5 "teaspoon" "salt"
            , rawPure 0.25 "teaspoon" "pepper"
            , rawPure 1.75 "cups" "Progresso\\8482 chicken broth (from 32-oz carton)"
            , rawMissing "cup" "milk"
            , rawPure 2.5 "cups" "shredded cooked chicken or turkey"
            , rawPure 2 "cups" "frozen mixed vegetables, thawed"
            ]
      in runExcept (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/chicken-pot-pie-pillsbury.txt")) `shouldBe` Right expected

    it "can parse taste of home" $
      let expected =
            [ rawPure 2 "cups" "diced peeled potatoes"
            , rawPure 1.75 "cups" "sliced carrots"
            , rawPure 1 "cup" "butter, cubed"
            , rawPure (2 / 3) "cup" "chopped onion"
            , rawPure 1 "cup" "all-purpose flour"
            , rawPure 1.75 "teaspoons" "salt"
            , rawPure 1 "teaspoon" "dried thyme"
            , rawPure 0.75 "teaspoon" "pepper"
            , rawPure 3 "cups" "chicken broth"
            , rawPure 1.5 "cups" "whole milk"
            , rawPure 4 "cups" "cubed cooked chicken"
            , rawPure 1 "cup" "frozen peas"
            , rawPure 1 "cup" "frozen corn"
            , rawPure 4 "sheets" "refrigerated pie crust"
            ]
      in runExcept (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/chicken-pot-pie-tasteofhome.txt")) `shouldBe` Right expected
