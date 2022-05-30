module Scraper.Site where

import Prelude

import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.List (groupBy, sortOn)
import Data.Text (Text)
import Text.HTML.Scalpel ((//), (@:), (@=), Scraper, Selector)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Text.HTML.Scalpel as Scalpel

import Scraper.Types
  ( IngredientScraper(..), ScrapeInfo(..), ScrapeName(..), ScrapeVersion(..), SiteName(..)
  , StepScraper(..), UnparsedIngredient(..), UnparsedStep(..), inception
  )

ingredientScrapers :: HashMap SiteName IngredientScraper
ingredientScrapers = HashMap.fromList
  [ ("allrecipes.com", allrecipesI)

  , ("cooking.nytimes.com", nytimesI)

  , ("food.com", geniusKitchen2I)
  , ("geniuskitchen.com", geniusKitchen1I)
  , ("tasteofhome.com", geniusKitchen1I)

  , ("rachlmansfield.com", tastyI2)
  , ("cookieandkate.com", tastyI1)
  , ("simpleveganblog.com", tastyI1)
  , ("eatyourselfskinny.com", tastyI1)
  , ("lexiscleankitchen.com", tastyI2)
  , ("sallysbakingaddiction.com", tastyI2)
  , ("gimmesomeoven.com", tastyI2)
  , ("pinchofyum.com", tastyI2)
  , ("alexandracooks.com", tastyI3)
  , ("naturallyella.com", tastyI3)
  , ("brownedbutterblondie.com", tastyI3)

  , ("foodnetwork.com", foodNetworkI)

  , ("cafedelites.com", wprmI)
  , ("budgetbytes.com", wprmI)
  , ("daringgourmet.com", wprmI)
  , ("recipetineats.com", wprmI)
  , ("cookingclassy.com", wprmI)
  , ("natashaskitchen.com", wprmI)
  , ("justonecookbook.com", wprmI)
  , ("loveandlemons.com", wprmI)
  , ("foodiecrush.com", wprmI)
  , ("therecipecritic.com", wprmI)
  , ("ambitiouskitchen.com", wprmI)
  , ("halfbakedharvest.com", wprmI)
  , ("ohsweetbasil.com", wprmI)
  , ("myfoodstory.com", wprmI)
  , ("easypeasyfoodie.com", wprmI)
  , ("veganricha.com", wprmI)
  , ("simplydeliciousfood.com", wprmI)
  , ("deliciouseveryday.com", wprmI)
  , ("iamafoodblog.com", wprmI)
  , ("thelastfoodblog.com", wprmI)
  , ("thefoodblog.net", wprmI)
  , ("onceuponafoodblog.com", wprmI)
  , ("anotherfoodblogger.com", wprmI)
  , ("minimalistbaker.com", wprmI)
  , ("davidlebovitz.com", wprmI)
  , ("skinnytaste.com", wprmI)
  , ("twopeasandtheirpod.com", wprmI)
  , ("sweetandsavorymeals.com", wprmI)
  , ("melskitchencafe.com", wprmI)
  , ("glutenfreecuppatea.co.uk", wprmI)

  , ("101cookbooks.com", cb101I)

  , ("bakerella.com", mvI)

  , ("localmilkblog.com", zlI)

  , ("smittenkitchen.com", jetpackI)

  , ("eatingwell.com", eatingWellI)
  , ("bhg.com", eatingWellI)

  , ("yummly.com", yummlyI)

  , ("simplyrecipes.com", simplyRecipesI)
  , ("seriouseats.com", simplyRecipesI)

  , ("bettycrocker.com", ingredientLi1)
  , ("pillsbury.com", ingredientLi1)
  , ("tasty.co", ingredientLi1)
  , ("lazycatkitchen.com", ingredientLi2)
  , ("deliciouslyella.com", ingredientLi3)
  , ("cookingandcooking.com", ingredientLi5)
  , ("damndelicious.net", ingredientLi6)
  , ("hemsleyandhemsley.com", ingredientLi6)
  , ("slenderkitchen.com", ingredientLi7)
  , ("everydayannie.com", ingredientLi8)
  , ("notwithoutsalt.com", ingredientLi9)
  , ("chefspencil.com", ingredientLi10)
  , ("shutterbean.com", ingredientLi11)
  , ("uitpaulineskeuken.nl", ingredientLi13)
  , ("leukerecepten.nl", ingredientLi14)
  , ("wsj.com", ingredientLi15)

  , ("delish.com", delishI)
  , ("thepioneerwoman.com", delishI)

  -- , ("epicurious.com", epicuriousI)

  , ("spoonacular.com", spoonacularI)

  , ("food52.com", food52I)

  , ("thekitchn.com", thekitchnI)

  , ("eatwell101.com", eatwell101I)

  , ("bbcgoodfood.com", bbcGoodFoodI)
  ]

stepScrapers :: HashMap SiteName StepScraper
stepScrapers = HashMap.fromList
  [ ("allrecipes.com", allrecipesS)

  , ("cooking.nytimes.com", nytimesS)

  , ("food.com", geniusKitchen2S)
  , ("geniuskitchen.com", geniusKitchen1S)
  , ("tasteofhome.com", geniusKitchen1S)

  , ("rachlmansfield.com", tastyS1)
  , ("cookieandkate.com", tastyS1)
  , ("simpleveganblog.com", tastyS1)
  , ("eatyourselfskinny.com", tastyS1)
  , ("lexiscleankitchen.com", tastyS1)
  , ("sallysbakingaddiction.com", tastyS2)
  , ("gimmesomeoven.com", tastyS2)
  , ("pinchofyum.com", tastyS2)
  , ("alexandracooks.com", tastyS2)
  , ("naturallyella.com", tastyS2)
  , ("brownedbutterblondie.com", tastyS3)
  , ("simple-veganista.com", tastyS4)

  , ("foodnetwork.com", foodNetworkS)

  , ("cafedelites.com", wprmS)
  , ("budgetbytes.com", wprmS)
  , ("daringgourmet.com", wprmS)
  , ("recipetineats.com", wprmS)
  , ("cookingclassy.com", wprmS)
  , ("natashaskitchen.com", wprmS)
  , ("justonecookbook.com", wprmS)
  , ("loveandlemons.com", wprmS)
  , ("foodiecrush.com", wprmS)
  , ("therecipecritic.com", wprmS)
  , ("ambitiouskitchen.com", wprmS)
  , ("halfbakedharvest.com", wprmS)
  , ("ohsweetbasil.com", wprmS)
  , ("myfoodstory.com", wprmS)
  , ("easypeasyfoodie.com", wprmS)
  , ("veganricha.com", wprmS)
  , ("simplydeliciousfood.com", wprmS)
  , ("deliciouseveryday.com", wprmS)
  , ("iamafoodblog.com", wprmS)
  , ("thelastfoodblog.com", wprmS)
  , ("thefoodblog.net", wprmS)
  , ("onceuponafoodblog.com", wprmS)
  , ("anotherfoodblogger.com", wprmS)
  , ("minimalistbaker.com", wprmS)
  , ("davidlebovitz.com", wprmS)
  , ("skinnytaste.com", wprmS)
  , ("twopeasandtheirpod.com", wprmS)
  , ("sweetandsavorymeals.com", wprmS)
  , ("melskitchencafe.com", wprmS)
  , ("glutenfreecuppatea.co.uk", wprmS)

  , ("101cookbooks.com", cb101S)

  , ("bakerella.com", mvS)

  , ("localmilkblog.com", zlS)

  , ("smittenkitchen.com", jetpackS)

  , ("eatingwell.com", eatingWellS)
  , ("bhg.com", eatingWellS)

  , ("yummly.com", yummlyS)

  , ("simplyrecipes.com", simplyRecipesS)
  , ("seriouseats.com", simplyRecipesS)

  , ("bettycrocker.com", stepLi1)
  , ("pillsbury.com", stepLi1)
  , ("tasty.co", stepLi2)
  , ("damndelicious.net", stepLi3)
  , ("lazycatkitchen.com", stepLi4)
  , ("deliciouslyella.com", stepLi5)
  , ("slenderkitchen.com", stepLi6)
  , ("everydayannie.com", stepLi7)
  , ("hemsleyandhemsley.com", stepLi8)
  , ("notwithoutsalt.com", stepLi9)
  , ("cookingandcooking.com", stepLi10)
  , ("uitpaulineskeuken.nl", stepLi11)
  , ("leukerecepten.nl", stepLi12)
  , ("wsj.com", stepLi13)
  , ("eatfigsnotpigs.com", stepLi14)

  , ("delish.com", delishS)
  , ("thepioneerwoman.com", delishS)

  , ("food52.com", food52S)

  , ("thekitchn.com", thekitchnS)

  , ("bbcgoodfood.com", bbcGoodFoodS)
  ]

-- |Get all ingredient scrapers, ordered by most popular first.
allIngredientScrapers :: [IngredientScraper]
allIngredientScrapers = fmap head . reverse . sortOn length  . groupBy ((==) `on` (scrapeInfoName . ingredientScraperInfo)) . sortOn (scrapeInfoName . ingredientScraperInfo) . HashMap.elems $ ingredientScrapers

-- |Get all step scrapers, ordered by most popular first.
allStepScrapers :: [StepScraper]
allStepScrapers = fmap head . reverse . sortOn length  . groupBy ((==) `on` (scrapeInfoName . stepScraperInfo)) . sortOn (scrapeInfoName . stepScraperInfo) . HashMap.elems $ stepScrapers

testScrape :: Selector -> Scraper Text Bool
testScrape test = not . Text.null <$> Scalpel.html test

acceptAll :: Scraper Text Bool
acceptAll = pure True

denyAll :: Scraper Text Bool
denyAll = pure True

setIngredientVersion :: Int -> IngredientScraper -> IngredientScraper
setIngredientVersion v scraper = scraper { ingredientScraperInfo = (ingredientScraperInfo scraper) { scrapeInfoVersion = ScrapeVersion v } }

setStepVersion :: Int -> StepScraper -> StepScraper
setStepVersion v scraper = scraper { stepScraperInfo = (stepScraperInfo scraper) { scrapeInfoVersion = ScrapeVersion v } }

simpleIngredientScraper :: Text -> Scraper Text Bool -> Selector -> IngredientScraper
simpleIngredientScraper sName test select = IngredientScraper (ScrapeInfo (ScrapeName sName) inception) test scrape
  where
    scrape = Scalpel.chroots select (
      UnparsedIngredientRaw
        <$> Scalpel.text Scalpel.anySelector
      )

simpleStepScraper :: Text -> Scraper Text Bool -> Selector -> StepScraper
simpleStepScraper sName test select = StepScraper (ScrapeInfo (ScrapeName sName) inception) test scrape
  where
    scrape = Scalpel.chroots select (
      UnparsedStepRaw
        <$> Scalpel.text Scalpel.anySelector
      )

debugI :: IngredientScraper
debugI = simpleIngredientScraper "debug" (testScrape "div") "div"

debugS :: StepScraper
debugS = simpleStepScraper "debug" (testScrape "div") "div"

allrecipesI :: IngredientScraper
allrecipesI = setIngredientVersion 2 $ simpleIngredientScraper "allrecipes"
  (testScrape ("meta" @: ["content" @= "Allrecipes"]))
  ("span" @: [Scalpel.hasClass "ingredients-item-name"])

allrecipesS :: StepScraper
allrecipesS = simpleStepScraper "allrecipes"
  (testScrape ("meta" @: ["content" @= "Allrecipes"]))
  ("ul" @: [Scalpel.hasClass "instructions-section"] // "div" @: [Scalpel.hasClass "section-body"])

nytimesI :: IngredientScraper
nytimesI = simpleIngredientScraper "nytimes"
  (testScrape ("meta" @: ["content" @= "NYT Cooking"]))
  ("ul" @: [Scalpel.hasClass "recipe-ingredients"] // "li")

nytimesS :: StepScraper
nytimesS = simpleStepScraper "nytimes"
  (testScrape ("meta" @: ["content" @= "NYT Cooking"]))
  ("ol" @: [Scalpel.hasClass "recipe-steps"] // "li")

geniusKitchen1I :: IngredientScraper
geniusKitchen1I = setIngredientVersion 2 $ simpleIngredientScraper "geniusKitchen1"
  (testScrape ("div" @: [Scalpel.hasClass "recipe-ingredients"]))
  ("div" @: [Scalpel.hasClass "recipe-ingredients"] // "li")

geniusKitchen2I :: IngredientScraper
geniusKitchen2I = setIngredientVersion 2 $ simpleIngredientScraper "geniusKitchen2"
  (testScrape ("ul" @: [Scalpel.hasClass "ingredients"]))
  ("ul" @: [Scalpel.hasClass "ingredients"] // "li")

geniusKitchen1S :: StepScraper
geniusKitchen1S = setStepVersion 2 $ simpleStepScraper "geniusKitchen1"
  (testScrape ("div" @: [Scalpel.hasClass "recipe-directions"]))
  ("div" @: [Scalpel.hasClass "recipe-directions"] // "li")

geniusKitchen2S :: StepScraper
geniusKitchen2S = setStepVersion 2 $ simpleStepScraper "geniusKitchen2"
  (testScrape ("ul" @: [Scalpel.hasClass "directions"]))
  ("ul" @: [Scalpel.hasClass "directions"] // "li")

tastyI1 :: IngredientScraper
tastyI1 = simpleIngredientScraper "tasty1"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipe-ingredients"]))
  ("div" @: [Scalpel.hasClass "tasty-recipe-ingredients"] // "li")

tastyI2 :: IngredientScraper
tastyI2 = simpleIngredientScraper "tasty2"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"]))
  ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"] // "li")

tastyI3 :: IngredientScraper
tastyI3 = simpleIngredientScraper "tasty3"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"]))
  ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"] // "p")

tastyS1 :: StepScraper
tastyS1 = simpleStepScraper "tasty1"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipe-instructions"]))
  ("div" @: [Scalpel.hasClass "tasty-recipe-instructions"] // "li")

tastyS2 :: StepScraper
tastyS2 = simpleStepScraper "tasty2"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipes-instructions"]))
  ("div" @: [Scalpel.hasClass "tasty-recipes-instructions"] // "li")

tastyS3 :: StepScraper
tastyS3 = simpleStepScraper "tasty3"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipes-instructions"]))
  ("div" @: [Scalpel.hasClass "tasty-recipes-instructions"] // "div" @: [Scalpel.hasClass "tasty-recipes-instructions-body"])

tastyS4 :: StepScraper
tastyS4 = simpleStepScraper "tasty4"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipe-instructions"]))
  ("div" @: [Scalpel.hasClass "tasty-recipe-instructions"] // "p")

foodNetworkI :: IngredientScraper
foodNetworkI = IngredientScraper (ScrapeInfo (ScrapeName "foodNetwork") inception) denyAll $
  fmap UnparsedIngredientRaw . filter (not . (==) "Deselect All") <$> Scalpel.chroots ("div" @: [Scalpel.hasClass "o-Ingredients__m-Body"] // "span") (Scalpel.text Scalpel.anySelector)

foodNetworkS :: StepScraper
foodNetworkS = StepScraper (ScrapeInfo (ScrapeName "foodNetwork") inception) denyAll $ do
  fmap UnparsedStepRaw <$> Scalpel.chroots ("div" @: [Scalpel.hasClass "o-Method__m-Body"] // "li") (Scalpel.text Scalpel.anySelector)

wprmI :: IngredientScraper
wprmI = simpleIngredientScraper "wprm"
  (testScrape ("div" @: [Scalpel.hasClass "wprm-recipe"]))
  ("li" @: [Scalpel.hasClass "wprm-recipe-ingredient"])

wprmS :: StepScraper
wprmS = simpleStepScraper "wprm"
  (testScrape ("div" @: [Scalpel.hasClass "wprm-recipe"]))
  ("li" @: [Scalpel.hasClass "wprm-recipe-instruction"])

cb101I :: IngredientScraper
cb101I = simpleIngredientScraper "cb101"
  (testScrape ("div" @: [Scalpel.hasClass "cb101-recipe-main"]))
  ("li" @: [Scalpel.hasClass "cb101-recipe-ingredient"])

cb101S :: StepScraper
cb101S = simpleStepScraper "cb101"
  (testScrape ("div" @: [Scalpel.hasClass "cb101-recipe-main"]))
  ("li" @: [Scalpel.hasClass "cb101-recipe-instruction"])

mvI :: IngredientScraper
mvI = simpleIngredientScraper "mv"
  (testScrape ("div" @: [Scalpel.hasClass "mv-create-ingredients"]))
  ("div" @: [Scalpel.hasClass "mv-create-ingredients"] // "li")

mvS :: StepScraper
mvS = simpleStepScraper "mv"
  (testScrape ("div" @: [Scalpel.hasClass "mv-create-instructions"]))
  ("div" @: [Scalpel.hasClass "mv-create-instructions"] // "li")

zlI :: IngredientScraper
zlI = simpleIngredientScraper "zl"
  (testScrape ("ul" @: ["id" @= "zlrecipe-ingredients-list"]))
  ("ul" @: ["id" @= "zlrecipe-ingredients-list"] // "li")

zlS :: StepScraper
zlS = simpleStepScraper "zl"
  (testScrape ("ol" @: ["id" @= "zlrecipe-instructions-list"]))
  ("ol" @: ["id" @= "zlrecipe-instructions-list"] // "li")

jetpackI :: IngredientScraper
jetpackI = simpleIngredientScraper "jetpack"
  (testScrape ("div" @: [Scalpel.hasClass "jetpack-recipe"]))
  ("div" @: [Scalpel.hasClass "jetpack-recipe-ingredients"] // "li")

jetpackS :: StepScraper
jetpackS = simpleStepScraper "jetpack"
  (testScrape ("div" @: [Scalpel.hasClass "jetpack-recipe"]))
  ("div" @: [Scalpel.hasClass "jetpack-recipe-directions"])

eatingWellI :: IngredientScraper
eatingWellI = simpleIngredientScraper "eatingWell"
  (testScrape ("ul" @: [Scalpel.hasClass "ingredients-section"]))
  ("ul" @: [Scalpel.hasClass "ingredients-section"] // "li")

eatingWellS :: StepScraper
eatingWellS = simpleStepScraper "eatingWell"
  (testScrape ("ul" @: [Scalpel.hasClass "instructions-section"]))
  ("ul" @: [Scalpel.hasClass "instructions-section"] // "li" // "p")

yummlyI :: IngredientScraper
yummlyI = simpleIngredientScraper "yummly"
  denyAll
  ("li" @: [Scalpel.hasClass "IngredientLine"])

yummlyS :: StepScraper
yummlyS = simpleStepScraper "yummly"
  denyAll
  ("li" @: [Scalpel.hasClass "prep-step"])

simplyRecipesI :: IngredientScraper
simplyRecipesI = simpleIngredientScraper "simplyrecipes"
  denyAll
  ("section" @: ["id" @= "section--ingredients_1-0"] // "li")

simplyRecipesS :: StepScraper
simplyRecipesS = simpleStepScraper "simplyrecipes"
  denyAll
  ("section" @: ["id" @= "section--instructions_1-0"] // "li")

ingredientLi1 :: IngredientScraper
ingredientLi1 = simpleIngredientScraper "ingredientLi1"
  (testScrape ("li" @: [Scalpel.hasClass "ingredient"]))
  ("li" @: [Scalpel.hasClass "ingredient"])

ingredientLi2 :: IngredientScraper
ingredientLi2 = simpleIngredientScraper "ingredientLi2"
  (testScrape ("div" @: [Scalpel.hasClass "ingredients-section"]))
  ("div" @: [Scalpel.hasClass "ingredients-section"] // "li")

ingredientLi3 :: IngredientScraper
ingredientLi3 = simpleIngredientScraper "ingredientLi3"
  (testScrape ("li" @: ["itemprop" @= "recipeIngredient"]))
  ("li" @: ["itemprop" @= "recipeIngredient"])

ingredientLi4 :: IngredientScraper
ingredientLi4 = simpleIngredientScraper "ingredientLi4"
  (testScrape ("div" @: [Scalpel.hasClass "ingredients"]))
  ("div" @: [Scalpel.hasClass "ingredients"] // "li")

ingredientLi5 :: IngredientScraper
ingredientLi5 = simpleIngredientScraper "ingredientLi5"
  (testScrape ("div" @: [Scalpel.hasClass "listIngredient"]))
  ("div" @: [Scalpel.hasClass "listIngredient"] // "li")

ingredientLi6 :: IngredientScraper
ingredientLi6 = simpleIngredientScraper "ingredientLi6"
  (testScrape ("li" @: ["itemprop" @= "ingredients"]))
  ("li" @: ["itemprop" @= "ingredients"])

ingredientLi7 :: IngredientScraper
ingredientLi7 = simpleIngredientScraper "ingredientLi7"
  (testScrape ("ul" @: [Scalpel.hasClass "ingredients"]))
  ("ul" @: [Scalpel.hasClass "ingredients"] // "li")

ingredientLi8 :: IngredientScraper
ingredientLi8 = simpleIngredientScraper "ingredientLi8"
  (testScrape ("div" @: [Scalpel.hasClass "ingredient-text"]))
  ("div" @: [Scalpel.hasClass "ingredient-text"] // "ul" // "li")

ingredientLi9 :: IngredientScraper
ingredientLi9 = simpleIngredientScraper "ingredientLi9"
  (testScrape ("div" @: [Scalpel.hasClass "cookbook-container-ingredients"]))
  ("p" @: [Scalpel.hasClass "cookbook-ingredient-item"])

ingredientLi10 :: IngredientScraper
ingredientLi10 = simpleIngredientScraper "ingredientLi10"
  (testScrape ("table" @: [Scalpel.hasClass "ingredients-table"]))
  ("table" @: [Scalpel.hasClass "ingredients-table"] // "tr")

ingredientLi11 :: IngredientScraper
ingredientLi11 = simpleIngredientScraper "ingredientLi11"
  (testScrape ("blockquote" @: [Scalpel.hasClass "recipe-block"]))
  ("blockquote" @: [Scalpel.hasClass "recipe-block"] // "li")

ingredientLi12 :: IngredientScraper
ingredientLi12 = simpleIngredientScraper "ingredientLi12"
  (testScrape ("li" @: [Scalpel.hasClass "structured-ingredients__list-item"]))
  ("li" @: [Scalpel.hasClass "structured-ingredients__list-item"])

ingredientLi13 :: IngredientScraper
ingredientLi13 = simpleIngredientScraper "ingredientLi13"
  (testScrape ("section" @: [Scalpel.hasClass "ingredients-list"]))
  ("section" @: [Scalpel.hasClass "ingredients-list"] // "li")

ingredientLi14 :: IngredientScraper
ingredientLi14 = simpleIngredientScraper "ingredientLi14"
  (testScrape ("ul" @: [Scalpel.hasClass "page-content__ingredients-list"]))
  ("ul" @: [Scalpel.hasClass "page-content__ingredients-list"] // "li")

ingredientLi15 :: IngredientScraper
ingredientLi15 = simpleIngredientScraper "ingredientLi15"
  (testScrape ("ul" @: [Scalpel.hasClass "ingredients-list"]))
  ("ul" @: [Scalpel.hasClass "ingredients-list"] // "li")

stepLi1 :: StepScraper
stepLi1 = simpleStepScraper "stepLi1"
  (testScrape ("ul" @: [Scalpel.hasClass "recipeSteps"]))
  ("ul" @: [Scalpel.hasClass "recipeSteps"] // "li")

stepLi2 :: StepScraper
stepLi2 = simpleStepScraper "stepLi2"
  (testScrape ("ol" @: [Scalpel.hasClass "prep-steps"]))
  ("ol" @: [Scalpel.hasClass "prep-steps"] // "li")

stepLi3 :: StepScraper
stepLi3 = simpleStepScraper "stepLi3"
  (testScrape ("div" @: [Scalpel.hasClass "instructions"]))
  ("div" @: [Scalpel.hasClass "instructions"] // "li")

stepLi4 :: StepScraper
stepLi4 = simpleStepScraper "stepLi4"
  (testScrape ("div" @: [Scalpel.hasClass "method-section"]))
  ("div" @: [Scalpel.hasClass "method-section"] // "li")

stepLi5 :: StepScraper
stepLi5 = simpleStepScraper "stepLi5"
  (testScrape ("section" @: [Scalpel.hasClass "recipe__section"]))
  ("section" @: [Scalpel.hasClass "recipe__section"] // "li")

stepLi6 :: StepScraper
stepLi6 = simpleStepScraper "stepLi6"
  (testScrape ("div" @: [Scalpel.hasClass "step-instructions"]))
  ("div" @: [Scalpel.hasClass "step-instructions"] // "div" @: [Scalpel.hasClass "step"])

stepLi7 :: StepScraper
stepLi7 = simpleStepScraper "stepLi7"
  (testScrape ("div" @: ["id" @= "directions"]))
  ("div" @: ["id" @= "directions"] // "li")

stepLi8 :: StepScraper
stepLi8 = simpleStepScraper "stepLi8"
  (testScrape ("div" @: [Scalpel.hasClass "recipe-main-copy"]))
  ("div" @: [Scalpel.hasClass "recipe-main-copy"])

stepLi9 :: StepScraper
stepLi9 = simpleStepScraper "stepLi9"
  (testScrape ("p" @: [Scalpel.hasClass "cookbook-instruction-item"]))
  ("p" @: [Scalpel.hasClass "cookbook-instruction-item"])

stepLi10 :: StepScraper
stepLi10 = simpleStepScraper "stepLi10"
  (testScrape ("p" @: [Scalpel.hasClass "recipeText"]))
  ("p" @: [Scalpel.hasClass "recipeText"])

stepLi11 :: StepScraper
stepLi11 = simpleStepScraper "stepLi11"
  (testScrape ("div" @: [Scalpel.hasClass "preparation-list"]))
  ("div" @: [Scalpel.hasClass "preparation-list"] // "li")

stepLi12 :: StepScraper
stepLi12 = simpleStepScraper "stepLi12"
  (testScrape ("div" @: [Scalpel.hasClass "page-content__recipe"]))
  ("div" @: [Scalpel.hasClass "page-content__recipe"] // "div" @: [Scalpel.hasClass "step"])

stepLi13 :: StepScraper
stepLi13 = simpleStepScraper "stepLi13"
  (testScrape ("ol" @: [Scalpel.hasClass "steps-list"]))
  ("ol" @: [Scalpel.hasClass "steps-list"] // "li")

stepLi14 :: StepScraper
stepLi14 = setStepVersion 2 $ simpleStepScraper "stepLi14"
  (testScrape ("li" @: [Scalpel.hasClass "instruction"]))
  ("li" @: [Scalpel.hasClass "instruction"])

delishI :: IngredientScraper
delishI = simpleIngredientScraper "delish"
  acceptAll
  ("div" @: [Scalpel.hasClass "ingredient-item"])

delishS :: StepScraper
delishS = simpleStepScraper "delish"
  acceptAll
  ("div" @: [Scalpel.hasClass "direction-lists"] // "li")

spoonacularI :: IngredientScraper
spoonacularI = simpleIngredientScraper "spoontacular"
  denyAll
  ("div" @: [Scalpel.hasClass "spoonacular-ingredient"])

food52I :: IngredientScraper
food52I = simpleIngredientScraper "food52"
  denyAll
  ("div" @: [Scalpel.hasClass "recipe__list--ingredients"] // "li")

food52S :: StepScraper
food52S = simpleStepScraper "food52"
  denyAll
  ("div" @: [Scalpel.hasClass "recipe__list--steps"] // "li")

-- epicuriousI :: IngredientScraper
-- epicuriousI = simpleIngredientScraper "epicurious"
--   denyAll
--   ("div" @: [Scalpel.hasClass "recipe__ingredient-list"])

thekitchnI :: IngredientScraper
thekitchnI = simpleIngredientScraper "thekitchn"
  denyAll
  ("ul" @: [Scalpel.hasClass "Recipe__ingredients"] // "li")

thekitchnS :: StepScraper
thekitchnS = simpleStepScraper "thekitchn"
  denyAll
  ("ol" @: [Scalpel.hasClass "Recipe__instructions"] // "li")

eatwell101I :: IngredientScraper
eatwell101I = simpleIngredientScraper "eatwell101"
  denyAll
  ("div" @: [Scalpel.hasClass "pf-content"] // "li")

bbcGoodFoodI :: IngredientScraper
bbcGoodFoodI = simpleIngredientScraper "bbcgoodfood"
  denyAll
  ("section" @: [Scalpel.hasClass "recipe__ingredients"] // "li")

bbcGoodFoodS :: StepScraper
bbcGoodFoodS = simpleStepScraper "bbcgoodfood"
  denyAll
  ("section" @: [Scalpel.hasClass "recipe__method-steps"] // "li")
