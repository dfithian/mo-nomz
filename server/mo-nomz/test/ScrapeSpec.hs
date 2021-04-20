module ScrapeSpec where

import ClassyPrelude

import Control.Monad (fail)
import Control.Monad.Except (runExceptT)
import Data.FileEmbed (embedFile)
import Network.URI (parseURI)
import System.FilePath.TH (fileRelativeToAbsoluteStr)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldMatchList)
import qualified Data.Attoparsec.Text as Atto

import ParsedIngredients
  ( allRecipesIngredients, cafeDelitesIngredients, foodIngredients, foodNetworkIngredients
  , pillsburyIngredients, rachelMansfieldIngredients, sallysBakingIngredients
  , tasteOfHomeIngredients
  )
import Types
  ( IngredientName(..), RawIngredient(..), RawQuantity(..), RawUnit(..), RecipeName(..), Ingredient
  )

import Scrape
import Scraper.Internal.Parser

parseStrict :: (Eq a, Show a) => a -> Atto.Parser a -> Text -> Expectation
parseStrict expected parser input = Atto.parseOnly parser input `shouldBe` Right expected

scrapeAndParse :: String -> String -> [Ingredient] -> Expectation
scrapeAndParse url expectedName expected = do
  uri <- maybe (fail "Invalid URL") pure $ parseURI url
  ScrapedRecipe {..} <- either (fail . unpack) pure =<< runExceptT (scrapeUrl uri)
  actual <- either (fail . unpack) pure =<< runExceptT (parseIngredients scrapedRecipeContents)
  scrapedRecipeTitle `shouldBe` RecipeName (pack expectedName)
  actual `shouldMatchList` expected

spec :: Spec
spec = describe "Scrape" $ do
  describe "Examples" $ do
    it "can parse a unit" $ parseStrict (RawUnit "ounces") unitP " ounces "
    it "can parse a fraction" $ parseStrict (RawQuantity $ 1 / 3) quantityP "1/3"
    it "can parse an improper fraction" $ parseStrict (RawQuantity 1.5) quantityP "1-1/2"
    it "can parse an improper fraction with spaces" $ parseStrict (RawQuantity 1.5) quantityP "1 1/2"
    it "can parse a decimal" $ parseStrict (RawQuantity 0.25) quantityP "0.25"
    it "can parse a word" $ parseStrict (RawQuantityWord "half") quantityP "\nhalf\n"
    it "can parse an ingredient name" $ parseStrict (IngredientName "chicken") nameP " chicken"
    it "can parse \"chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") RawQuantityMissing RawUnitMissing) ingredientP "chicken"
    it "can parse \"one chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") (RawQuantityWord "one") RawUnitMissing) ingredientP "one chicken"
    it "can parse \"whole chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") RawQuantityMissing (RawUnit "whole")) ingredientP "whole chicken"
    it "can parse \"one whole chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") (RawQuantityWord "one") (RawUnit "whole")) ingredientP "one whole chicken"
    it "can parse \"1/4 cup broth\"" $ parseStrict (RawIngredient (IngredientName "broth") (RawQuantity 0.25) (RawUnit "cup")) ingredientP "1/4\ncup\nbroth"

  describe "Paste" $ do
    it "can parse allrecipes" $ do
      actual <- either (fail . unpack) pure =<< runExceptT (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/chicken-pot-pie-allrecipes.txt"))
      actual `shouldMatchList` allRecipesIngredients

    it "can parse pillsbury" $ do
      actual <- either (fail . unpack) pure =<< runExceptT (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/chicken-pot-pie-pillsbury.txt"))
      actual `shouldMatchList` pillsburyIngredients

    it "can parse taste of home" $ do
      actual <- either (fail . unpack) pure =<< runExceptT (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/chicken-pot-pie-tasteofhome.txt"))
      actual  `shouldMatchList` tasteOfHomeIngredients

    it "can parse rachel mansfield" $ do
      actual <- either (fail . unpack) pure =<< runExceptT (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/banana-bread-rachelmansfield.txt"))
      actual  `shouldMatchList` rachelMansfieldIngredients

    it "can parse food network" $ do
      actual <- either (fail . unpack) pure =<< runExceptT (parseIngredients $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "./fixtures/roast-chicken-food-network.txt"))
      actual `shouldMatchList` foodNetworkIngredients

  describe "Scrape" $ do
    it "can parse allrecipes" $
      scrapeAndParse
        "https://www.allrecipes.com/recipe/26317/chicken-pot-pie-ix/"
        "Chicken Pot Pie IX Recipe | Allrecipes"
        allRecipesIngredients

    it "can parse food" $
      scrapeAndParse
        "https://www.food.com/recipe/hearty-tuscan-white-bean-soup-192495#activity-feed"
        "Hearty Tuscan White Bean Soup Recipe  - Food.com"
        foodIngredients

    it "can parse pillsbury" $
      scrapeAndParse
        "https://www.pillsbury.com/recipes/classic-chicken-pot-pie/1401d418-ac0b-4b50-ad09-c6f1243fb992"
        "Classic Chicken Pot Pie Recipe - Pillsbury.com"
        pillsburyIngredients

    it "can parse taste of home" $
      scrapeAndParse
        "https://www.tasteofhome.com/recipes/favorite-chicken-potpie/"
        "Favorite Chicken Potpie Recipe: How to Make It | Taste of Home"
        tasteOfHomeIngredients

    it "can parse rachel mansfield" $
      scrapeAndParse
        "https://rachlmansfield.com/paleo-chocolate-chip-banana-bread/"
        "Paleo Chocolate Chip Banana Bread (Nut Free) - rachLmansfield"
        rachelMansfieldIngredients

    it "can parse food network" $
      scrapeAndParse
        "https://www.foodnetwork.com/recipes/ina-garten/perfect-roast-chicken-recipe-1940592"
        "Perfect Roast Chicken Recipe | Ina Garten | Food Network"
        foodNetworkIngredients

    it "can parse sallys baking" $
      scrapeAndParse
        "https://sallysbakingaddiction.com/chocolate-lava-cakes/"
        "How to Make Chocolate Lava Cakes | Sally's Baking Addiction"
        sallysBakingIngredients

    it "can parse cafe delites" $
      scrapeAndParse
        "https://cafedelites.com/chicken-tikka-masala/"
        "Chicken Tikka Masala - Cafe Delites"
        cafeDelitesIngredients
