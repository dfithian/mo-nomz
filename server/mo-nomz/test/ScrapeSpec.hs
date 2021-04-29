module ScrapeSpec where

import ClassyPrelude

import Control.Monad (fail)
import Control.Monad.Except (runExceptT)
import Network.URI (parseURI)
import Test.Hspec
  ( Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldMatchList, shouldSatisfy
  )
import qualified Data.CaseInsensitive as CI

import ParsedIngredients
  ( allRecipesIngredients, bettyCrockerIngredients, cafeDelitesIngredients, eatingWellIngredients
  , foodIngredients, foodNetworkIngredients, pillsburyIngredients, rachelMansfieldIngredients
  , sallysBakingIngredients, tasteOfHomeIngredients
  )
import TestEnv (Env(..))
import Types (Ingredient(..), IngredientName(..), Quantity(..), RecipeName(..), Unit(..))

import Scrape

data TestCfg = TestCfg
  { requireUnit :: Bool
  , env :: Env
  }

defaultTestCfg :: Env -> TestCfg
defaultTestCfg env = TestCfg
  { requireUnit = True
  , env = env
  }

scrapeAndParse :: Env -> String -> String -> [Ingredient] -> Expectation
scrapeAndParse Env {..} url expectedName expected = do
  uri <- maybe (fail "Invalid URL") pure $ parseURI url
  ScrapedRecipe {..} <- either (fail . unpack) pure =<< runReaderT (runExceptT (scrapeUrl uri)) envManager
  scrapedRecipeName `shouldBe` RecipeName (pack expectedName)
  scrapedRecipeIngredients `shouldMatchList` expected

scrapeAndParseConfig :: TestCfg -> String -> Expectation
scrapeAndParseConfig TestCfg {..} url = do
  let Env {..} = env
  uri <- maybe (fail "Invalid URL") pure $ parseURI url
  ScrapedRecipe {..} <- either (fail . unpack) pure =<< runReaderT (runExceptT (scrapeUrl uri)) envManager
  unRecipeName scrapedRecipeName `shouldSatisfy` not . null
  scrapedRecipeIngredients `shouldSatisfy` (\xs -> length xs >= 5)
  scrapedRecipeIngredients `shouldSatisfy` any hasQuantityAndUnit
  scrapedRecipeIngredients `shouldSatisfy` lessThanThreeDuplicates
  lessThanThreePrefixes scrapedRecipeIngredients
  where
    hasQuantityAndUnit Ingredient {..} = ingredientQuantity /= QuantityMissing && (if requireUnit then ingredientUnit /= UnitMissing else True)
    lessThanThreeDuplicates = (< 3) . length . filter ((> 1) . length . snd) . mapToList . foldr (\x@Ingredient {..} -> asMap . insertWith (<>) ingredientName [x]) mempty
    lessThanThreePrefixes xs = do
      let names = ingredientName <$> xs
          toStr = toLower . CI.original . unIngredientName
          go x y = case name `isPrefixOf` otherName && name /= otherName of
            True -> [unpack name <> " is a prefix of " <> unpack otherName]
            False -> []
            where
              name = toStr x
              otherName = toStr y
          prefixes = [ z | x <- names, y <- names, z <- go x y ]
      when (length prefixes >= 3) $
        expectationFailure $ intercalate ", " prefixes

spec :: Env -> Spec
spec env = describe "Scrape" $ do
  describe "Examples" $ do
    it "can parse allrecipes" $
      scrapeAndParse
        env
        "https://www.allrecipes.com/recipe/26317/chicken-pot-pie-ix/"
        "Chicken Pot Pie IX Recipe | Allrecipes"
        allRecipesIngredients

    it "can parse food" $
      scrapeAndParse
        env
        "https://www.food.com/recipe/hearty-tuscan-white-bean-soup-192495"
        "Hearty Tuscan White Bean Soup Recipe  - Food.com"
        foodIngredients

    it "can parse pillsbury" $
      scrapeAndParse
        env
        "https://www.pillsbury.com/recipes/classic-chicken-pot-pie/1401d418-ac0b-4b50-ad09-c6f1243fb992"
        "Classic Chicken Pot Pie Recipe - Pillsbury.com"
        pillsburyIngredients

    it "can parse betty crocker" $
      scrapeAndParse
        env
        "https://www.bettycrocker.com/recipes/mississippi-mud-brownies/dff02c0e-695b-4b01-90fd-7071ddb84457"
        "Mississippi Mud Brownies Recipe - BettyCrocker.com"
        bettyCrockerIngredients

    it "can parse taste of home" $
      scrapeAndParse
        env
        "https://www.tasteofhome.com/recipes/favorite-chicken-potpie/"
        "Favorite Chicken Potpie Recipe: How to Make It | Taste of Home"
        tasteOfHomeIngredients

    it "can parse rachel mansfield" $
      scrapeAndParse
        env
        "https://rachlmansfield.com/paleo-chocolate-chip-banana-bread/"
        "Paleo Chocolate Chip Banana Bread (Nut Free) - rachLmansfield"
        rachelMansfieldIngredients

    it "can parse food network" $
      scrapeAndParse
        env
        "https://www.foodnetwork.com/recipes/ina-garten/perfect-roast-chicken-recipe-1940592"
        "Perfect Roast Chicken Recipe | Ina Garten | Food Network"
        foodNetworkIngredients

    it "can parse sallys baking" $
      scrapeAndParse
        env
        "https://sallysbakingaddiction.com/chocolate-lava-cakes/"
        "How to Make Chocolate Lava Cakes | Sally's Baking Addiction"
        sallysBakingIngredients

    it "can parse cafe delites" $
      scrapeAndParse
        env
        "https://cafedelites.com/chicken-tikka-masala/"
        "Chicken Tikka Masala - Cafe Delites"
        cafeDelitesIngredients

    it "can parse eatingwell" $
      scrapeAndParse
        env
        "https://www.eatingwell.com/recipe/7898240/baked-spinach-feta-pasta/"
        "Baked Spinach & Feta Pasta Recipe | EatingWell"
        eatingWellIngredients

  describe "Smoke Test" $ do
    let defCfg = defaultTestCfg env
    it "handles nytimes" $ scrapeAndParseConfig defCfg "https://cooking.nytimes.com/recipes/1017256-french-onion-soup"
    it "handles yummly" $ scrapeAndParseConfig defCfg "https://www.yummly.com/recipe/Barbecue-Baked-Chicken-Legs-9073054"
    it "handles epicurious" $ scrapeAndParseConfig defCfg "https://www.epicurious.com/recipes/food/views/cashew-chicken"
    it "handles tasty" $ scrapeAndParseConfig defCfg "https://tasty.co/recipe/cilantro-lime-chicken-veggie-rice-meal-prep"
    it "handles delish" $ scrapeAndParseConfig defCfg "https://www.delish.com/cooking/recipe-ideas/a27469808/acai-bowl-recipe/"
    it "handles delish" $ scrapeAndParseConfig defCfg "https://www.delish.com/cooking/a36146989/vegan-tofu-grain-bowl/"
    it "handles spoonacular" $ scrapeAndParseConfig defCfg "https://spoonacular.com/recipes/chocolate-chip-cookie-bars-1518975"
    it "handles cookieandkate" $ scrapeAndParseConfig defCfg "https://cookieandkate.com/cream-of-broccoli-soup-recipe/"
    it "handles budgetbytes" $ scrapeAndParseConfig defCfg "https://www.budgetbytes.com/spaghetti-with-vegetable-meat-sauce/"
    it "handles daringgourmet" $ scrapeAndParseConfig defCfg "https://www.daringgourmet.com/hamburger-gravy/"
    it "handles damndelicious" $ scrapeAndParseConfig defCfg "https://damndelicious.net/2020/12/04/creamy-chicken-and-gnocchi/"
    it "handles gimmesomeoven" $ scrapeAndParseConfig defCfg "https://www.gimmesomeoven.com/best-caesar-salad/"
    it "handles recipetineats" $ scrapeAndParseConfig defCfg "https://www.recipetineats.com/orecchiette-sausage-pasta-in-creamy-tomato-sauce/"
    it "handles cookingclassy" $ scrapeAndParseConfig defCfg "https://www.cookingclassy.com/sheet-pan-shrimp-asparagus/"
    it "handles natashaskitchen" $ scrapeAndParseConfig defCfg "https://natashaskitchen.com/oven-roasted-baby-red-potatoes/"
    it "handles pinchofyum" $ scrapeAndParseConfig defCfg "https://pinchofyum.com/spicy-chicken-sweet-potato-meal-prep-magic"
    it "handles justonecookbook" $ scrapeAndParseConfig defCfg "https://www.justonecookbook.com/teriyaki-pork-donburi/"
    it "handles loveandlemons" $ scrapeAndParseConfig defCfg "https://www.loveandlemons.com/artichoke-dipping-sauce/"
    it "handles foodiecrush" $ scrapeAndParseConfig defCfg "https://www.foodiecrush.com/strawberry-and-avocado-spinach-salad-with-chicken/"
    it "handles therecipecritic" $ scrapeAndParseConfig defCfg "https://therecipecritic.com/hot-spinach-artichoke-dip/"
    it "handles ambitiouskitchen" $ scrapeAndParseConfig defCfg "https://www.ambitiouskitchen.com/coconut-chocolate-peanut-butter-protein-bars/"
    it "handles melskitchencafe" $ scrapeAndParseConfig defCfg "https://www.melskitchencafe.com/easy-chicken-enchilada-casserole/"
    it "handles halfbakedharvest" $ scrapeAndParseConfig defCfg "https://www.halfbakedharvest.com/southern-butter-biscuits/"
    it "handles simpleveganblog" $ scrapeAndParseConfig defCfg "https://simpleveganblog.com/vegan-stuffed-peppers/"
    it "handles smittenkitchen" $ scrapeAndParseConfig defCfg "https://smittenkitchen.com/2021/04/spring-asparagus-galette/"
    it "handles 101cookbooks" $ scrapeAndParseConfig defCfg "https://www.101cookbooks.com/mushroom-scallion-tartine/"
    it "handles ohsweetbasil" $ scrapeAndParseConfig defCfg "https://ohsweetbasil.com/quick-grilled-chicken-with-oregaon-recipe/"
    it "handles myfoodstory" $ scrapeAndParseConfig defCfg "https://myfoodstory.com/prawn-rava-fry/"
    it "handles easypeasyfoodie" $ scrapeAndParseConfig defCfg "https://www.easypeasyfoodie.com/coffee-and-walnut-traybake/"
    it "handles veganricha" $ scrapeAndParseConfig defCfg "https://www.veganricha.com/potato-quinoa-waffles-aloo-tikki-waffles/"
    it "handles simplydeliciousfood" $ scrapeAndParseConfig defCfg "https://simply-delicious-food.com/general-tsos-chicken/"
    it "handles lexiscleankitchen" $ scrapeAndParseConfig defCfg "https://lexiscleankitchen.com/buffalo-chicken-dip/"
    it "handles lazycatkitchen" $ scrapeAndParseConfig defCfg "https://www.lazycatkitchen.com/vegan-blondies/"
    it "handles alexandracooks" $ scrapeAndParseConfig defCfg "https://alexandracooks.com/2020/06/25/easy-homemade-pita-bread-recipe/"
    it "handles deliciouslyella" $ scrapeAndParseConfig defCfg "https://deliciouslyella.com/recipes/sweet-potato-black-bean-shepherds-pie/"
    it "handles deliciouseveryday" $ scrapeAndParseConfig (defCfg { requireUnit = False }) "https://www.deliciouseveryday.com/thai-pumpkin-soup-recipe/"
    it "handles eatyourselfskinny" $ scrapeAndParseConfig defCfg "https://www.eatyourselfskinny.com/easy-beef-stroganoff-casserole/"
    it "handles iamafoodblog" $ scrapeAndParseConfig defCfg "https://iamafoodblog.com/smashed-brussel-sprouts/"
    it "handles naturallyella" $ scrapeAndParseConfig defCfg "https://naturallyella.com/roasted-sweet-potato-sorghum-salad/"
    it "handles glutenfreecuppatea" $ scrapeAndParseConfig defCfg "https://glutenfreecuppatea.co.uk/2021/04/13/toblerone-millionaires-shortbread-recipe/"
    it "handles thelastfoodblog" $ scrapeAndParseConfig defCfg "https://www.thelastfoodblog.com/spinach-and-ricotta-cannelloni/"
    it "handles hemsleyandhemsley" $ scrapeAndParseConfig defCfg "https://hemsleyandhemsley.com/recipe/apple-rocket-and-feta-buckwheat-galettes/"
    it "handles localmilkblog" $ scrapeAndParseConfig defCfg "https://localmilkblog.com/2019/11/turkey-buttermilk-sage-dumpling-soup.html"
    it "handles thefoodblog" $ scrapeAndParseConfig defCfg "https://www.thefoodblog.net/air-fryer-salmon-recipe/"
    it "handles onceuponafoodblog" $ scrapeAndParseConfig defCfg "https://onceuponafoodblog.com/cheesy-bacon-spring-greens/"
    it "handles anotherfoodblogger" $ scrapeAndParseConfig defCfg "https://www.anotherfoodblogger.com/recipes/meatball-marinara/#wprm-recipe-container-6560"
    it "handles brownedbutterblondie" $ scrapeAndParseConfig defCfg "https://brownedbutterblondie.com/the-best-blueberry-streusel-muffins/#tasty-recipes-7789"
