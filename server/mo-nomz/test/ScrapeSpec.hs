module ScrapeSpec where

import ClassyPrelude

import Control.Monad (fail)
import Control.Monad.Except (runExceptT)
import Data.FileEmbed (embedFile)
import Network.URI (parseURI)
import System.FilePath.TH (fileRelativeToAbsoluteStr)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldMatchList, shouldSatisfy, expectationFailure)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.CaseInsensitive as CI

import ParsedIngredients
  ( allRecipesIngredients, bettyCrockerIngredients, cafeDelitesIngredients, eatingWellIngredients
  , foodIngredients, foodNetworkIngredients, pillsburyIngredients, rachelMansfieldIngredients
  , sallysBakingIngredients, tasteOfHomeIngredients
  )
import Types
  ( Ingredient(..), IngredientName(..), Quantity(..), RawIngredient(..), RawQuantity(..)
  , RawUnit(..), RecipeName(..), Unit(..)
  )

import Scrape
import Scraper.Internal.Parser

data TestCfg = TestCfg
  { requireUnit :: Bool
  }

defaultTestCfg :: TestCfg
defaultTestCfg = TestCfg
  { requireUnit = True
  }

parseStrict :: (Eq a, Show a) => a -> Atto.Parser a -> Text -> Expectation
parseStrict expected parser input = Atto.parseOnly parser input `shouldBe` Right expected

scrapeAndParse :: String -> String -> [Ingredient] -> Expectation
scrapeAndParse url expectedName expected = do
  uri <- maybe (fail "Invalid URL") pure $ parseURI url
  ScrapedRecipe {..} <- either (fail . unpack) pure =<< runExceptT (scrapeUrl uri)
  scrapedRecipeName `shouldBe` RecipeName (pack expectedName)
  scrapedRecipeIngredients `shouldMatchList` expected

scrapeAndParseBasic :: String -> Expectation
scrapeAndParseBasic = scrapeAndParseConfig defaultTestCfg

scrapeAndParseConfig :: TestCfg -> String -> Expectation
scrapeAndParseConfig TestCfg {..} url = do
  uri <- maybe (fail "Invalid URL") pure $ parseURI url
  ScrapedRecipe {..} <- either (fail . unpack) pure =<< runExceptT (scrapeUrl uri)
  unRecipeName scrapedRecipeName `shouldSatisfy` not . null
  scrapedRecipeIngredients `shouldSatisfy` testIngredients
  noInfixes scrapedRecipeIngredients
  where
    hasQuantityAndUnit Ingredient {..} = ingredientQuantity /= QuantityMissing && (if requireUnit then ingredientUnit /= UnitMissing else True)
    noDuplicates = null . filter ((> 1) . length . snd) . mapToList . foldr (\x@Ingredient {..} -> asMap . insertWith (<>) ingredientName [x]) mempty
    testIngredients xs = not (null xs) && any hasQuantityAndUnit xs && noDuplicates xs
    noInfixes xs = do
      let names = ingredientName <$> xs
          toStr = toLower . CI.original . unIngredientName
          go x y = case toStr x `isPrefixOf` toStr y && name /= otherName of
            True -> expectationFailure $ unpack (toStr x) <> " is a prefix of " <> unpack (toStr y)
            False -> pure ()
      for_ [(x, y) | x <- names, y <- names] $ uncurry go

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

    it "can parse betty crocker" $
      scrapeAndParse
        "https://www.bettycrocker.com/recipes/mississippi-mud-brownies/dff02c0e-695b-4b01-90fd-7071ddb84457"
        "Mississippi Mud Brownies Recipe - BettyCrocker.com"
        bettyCrockerIngredients

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

    it "can parse eatingwell" $
      scrapeAndParse
        "https://www.eatingwell.com/recipe/7898240/baked-spinach-feta-pasta/"
        "Baked Spinach & Feta Pasta Recipe | EatingWell"
        eatingWellIngredients

    -- START HERE
    it "handles yummly" $ scrapeAndParseBasic "https://www.yummly.com/recipe/Barbecue-Baked-Chicken-Legs-9073054"
    it "handles epicurious" $ scrapeAndParseBasic "https://www.epicurious.com/recipes/food/views/cashew-chicken"
    it "handles tasty" $ scrapeAndParseBasic "https://tasty.co/recipe/cilantro-lime-chicken-veggie-rice-meal-prep"
    it "handles delish" $ scrapeAndParseBasic "https://www.delish.com/cooking/recipe-ideas/a27469808/acai-bowl-recipe/"
    it "handles delish" $ scrapeAndParseBasic "https://www.delish.com/cooking/a36146989/vegan-tofu-grain-bowl/"
    it "handles spoonacular" $ scrapeAndParseBasic "https://spoonacular.com/recipes/chocolate-chip-cookie-bars-1518975"
    it "handles cookieandkate" $ scrapeAndParseBasic "https://cookieandkate.com/cream-of-broccoli-soup-recipe/"
    it "handles budgetbytes" $ scrapeAndParseBasic "https://www.budgetbytes.com/spaghetti-with-vegetable-meat-sauce/"
    it "handles daringgourmet" $ scrapeAndParseBasic "https://www.daringgourmet.com/hamburger-gravy/"
    it "handles damndelicious" $ scrapeAndParseBasic "https://damndelicious.net/2020/12/04/creamy-chicken-and-gnocchi/"
    it "handles gimmesomeoven" $ scrapeAndParseBasic "https://www.gimmesomeoven.com/best-caesar-salad/"
    it "handles recipetineats" $ scrapeAndParseBasic "https://www.recipetineats.com/orecchiette-sausage-pasta-in-creamy-tomato-sauce/"
    it "handles cookingclassy" $ scrapeAndParseBasic "https://www.cookingclassy.com/sheet-pan-shrimp-asparagus/"
    it "handles natashaskitchen" $ scrapeAndParseBasic "https://natashaskitchen.com/oven-roasted-baby-red-potatoes/"
    it "handles pinchofyum" $ scrapeAndParseBasic "https://pinchofyum.com/spicy-chicken-sweet-potato-meal-prep-magic"
    it "handles justonecookbook" $ scrapeAndParseBasic "https://www.justonecookbook.com/teriyaki-pork-donburi/"
    it "handles loveandlemons" $ scrapeAndParseBasic "https://www.loveandlemons.com/artichoke-dipping-sauce/"
    it "handles foodiecrush" $ scrapeAndParseBasic "https://www.foodiecrush.com/strawberry-and-avocado-spinach-salad-with-chicken/"
    it "handles therecipecritic" $ scrapeAndParseBasic "https://therecipecritic.com/hot-spinach-artichoke-dip/"
    it "handles ambitiouskitchen" $ scrapeAndParseBasic "https://www.ambitiouskitchen.com/coconut-chocolate-peanut-butter-protein-bars/"
    it "handles melskitchencafe" $ scrapeAndParseBasic "https://www.melskitchencafe.com/easy-chicken-enchilada-casserole/"
    it "handles halfbakedharvest" $ scrapeAndParseBasic "https://www.halfbakedharvest.com/southern-butter-biscuits/"
    it "handles simpleveganblog" $ scrapeAndParseBasic "https://simpleveganblog.com/vegan-stuffed-peppers/"
    it "handles smittenkitchen" $ scrapeAndParseBasic "https://smittenkitchen.com/2021/04/spring-asparagus-galette/"
    it "handles 101cookbooks" $ scrapeAndParseBasic "https://www.101cookbooks.com/citrus-recipes/"
    it "handles ohsweetbasil" $ scrapeAndParseBasic "https://ohsweetbasil.com/quick-grilled-chicken-with-oregaon-recipe/"
    it "handles myfoodstory" $ scrapeAndParseBasic "https://myfoodstory.com/prawn-rava-fry/"
    it "handles easypeasyfoodie" $ scrapeAndParseBasic "https://www.easypeasyfoodie.com/coffee-and-walnut-traybake/"
    it "handles veganricha" $ scrapeAndParseBasic "https://www.veganricha.com/potato-quinoa-waffles-aloo-tikki-waffles/"
    it "handles simplydeliciousfood" $ scrapeAndParseBasic "https://simply-delicious-food.com/general-tsos-chicken/"
    it "handles lexiscleankitchen" $ scrapeAndParseBasic "https://lexiscleankitchen.com/buffalo-chicken-dip/"
    it "handles lazycatkitchen" $ scrapeAndParseBasic "https://www.lazycatkitchen.com/vegan-blondies/"
    it "handles alexandracooks" $ scrapeAndParseBasic "https://alexandracooks.com/2020/06/25/easy-homemade-pita-bread-recipe/"
    it "handles deliciouslyella" $ scrapeAndParseBasic "https://deliciouslyella.com/recipes/sweet-potato-black-bean-shepherds-pie/"
    it "handles deliciouseveryday" $ scrapeAndParseConfig (defaultTestCfg { requireUnit = False }) "https://www.deliciouseveryday.com/thai-pumpkin-soup-recipe/"
    it "handles eatyourselfskinny" $ scrapeAndParseBasic "https://www.eatyourselfskinny.com/easy-beef-stroganoff-casserole/"
    it "handles iamafoodblog" $ scrapeAndParseBasic "https://iamafoodblog.com/smashed-brussel-sprouts/"
    it "handles naturallyella" $ scrapeAndParseBasic "https://naturallyella.com/roasted-sweet-potato-sorghum-salad/"
    it "handles glutenfreecuppatea" $ scrapeAndParseBasic "https://glutenfreecuppatea.co.uk/2021/04/13/toblerone-millionaires-shortbread-recipe/"
    it "handles thelastfoodblog" $ scrapeAndParseBasic "https://www.thelastfoodblog.com/spinach-and-ricotta-cannelloni/"
    it "handles hemsleyandhemsley" $ scrapeAndParseBasic "https://hemsleyandhemsley.com/recipe/apple-rocket-and-feta-buckwheat-galettes/"
    it "handles localmilkblog" $ scrapeAndParseBasic "https://localmilkblog.com/2019/11/turkey-buttermilk-sage-dumpling-soup.html"
    it "handles thefoodblog" $ scrapeAndParseBasic "https://www.thefoodblog.net/air-fryer-salmon-recipe/"
    it "handles onceuponafoodblog" $ scrapeAndParseBasic "https://onceuponafoodblog.com/cheesy-bacon-spring-greens/"
    it "handles anotherfoodblogger" $ scrapeAndParseBasic "https://www.anotherfoodblogger.com/recipes/meatball-marinara/#wprm-recipe-container-6560"
    it "handles brownedbutterblondie" $ scrapeAndParseBasic "https://brownedbutterblondie.com/the-best-blueberry-streusel-muffins/#tasty-recipes-7789"
