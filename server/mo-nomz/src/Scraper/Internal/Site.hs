module Scraper.Internal.Site where

import ClassyPrelude
import Text.HTML.Scalpel ((//), (@:), (@=), Scraper, Selector)
import qualified Data.HashMap.Strict as HashMap
import qualified Text.HTML.Scalpel as Scalpel

import Scraper.Internal.Types
  ( SiteName(..), SiteScraper(..), UnparsedIngredient(..), UnparsedStep(..)
  )

siteScrapers :: HashMap SiteName SiteScraper
siteScrapers = mapFromList
  [ ("allrecipes.com", allrecipes)

  , ("cooking.nytimes.com", nytimes)

  , ("food.com", geniusKitchen1)
  , ("geniuskitchen.com", geniusKitchen1)
  , ("tasteofhome.com", geniusKitchen2)

  , ("rachlmansfield.com", tasty2)
  , ("cookieandkate.com", tasty1)
  , ("simpleveganblog.com", tasty1)
  , ("eatyourselfskinny.com", tasty1)
  , ("lexiscleankitchen.com", tasty2)
  , ("sallysbakingaddiction.com", tasty2)
  , ("gimmesomeoven.com", tasty2)
  , ("pinchofyum.com", tasty2)
  , ("alexandracooks.com", tasty3)
  , ("naturallyella.com", tasty3)
  , ("brownedbutterblondie.com", tasty3)

  , ("foodnetwork.com", foodNetwork)

  , ("cafedelites.com", wprm)
  , ("budgetbytes.com", wprm)
  , ("daringgourmet.com", wprm)
  , ("recipetineats.com", wprm)
  , ("cookingclassy.com", wprm)
  , ("natashaskitchen.com", wprm)
  , ("justonecookbook.com", wprm)
  , ("loveandlemons.com", wprm)
  , ("foodiecrush.com", wprm)
  , ("therecipecritic.com", wprm)
  , ("ambitiouskitchen.com", wprm)
  , ("halfbakedharvest.com", wprm)
  , ("101cookbooks.com", wprm)
  , ("ohsweetbasil.com", wprm)
  , ("myfoodstory.com", wprm)
  , ("easypeasyfoodie.com", wprm)
  , ("veganricha.com", wprm)
  , ("simplydeliciousfood.com", wprm)
  , ("deliciouseveryday.com", wprm)
  , ("iamafoodblog.com", wprm)
  , ("thelastfoodblog.com", wprm)
  , ("thefoodblog.net", wprm)
  , ("onceuponafoodblog.com", wprm)
  , ("anotherfoodblogger.com", wprm)
  , ("minimalistbaker.com", wprm)
  , ("davidlebovitz.com", wprm)
  , ("skinnytaste.com", wprm)
  , ("twopeasandtheirpod.com", wprm)
  , ("sweetandsavorymeals.com", wprm)
  , ("melskitchencafe.com", wprm)

  , ("glutenfreecuppatea.co.uk", mv)

  , ("localmilkblog.com", zl)

  , ("smittenkitchen.com", jetpack)

  , ("eatingwell.com", eatingWell)
  , ("bhg.com", eatingWell)

  , ("yummly.com", yummly)

  , ("simplyrecipes.com", simplyRecipes)

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
  , ("seriouseats.com", ingredientLi12)
  , ("uitpaulineskeuken.nl", ingredientLi13)
  , ("leukerecepten.nl", ingredientLi14)

  , ("delish.com", delish)
  , ("thepioneerwoman.com", delish)

  -- , ("epicurious.com", epicurious)

  , ("spoonacular.com", spoonacular)

  , ("food52.com", food52)

  , ("thekitchn.com", thekitchn)

  , ("eatwell101.com", eatwell101)
  ]

-- |Get all site scrapers, ordered by most popular first.
allSiteScrapers :: [SiteScraper]
allSiteScrapers = map unsafeHead . reverse . sortOn length  . groupBy ((==) `on` siteScraperName) . sortOn siteScraperName . HashMap.elems $ siteScrapers

testScrape :: Selector -> Scraper Text Bool
testScrape test = not . null <$> Scalpel.html test

acceptAll :: Scraper Text Bool
acceptAll = pure True

denyAll :: Scraper Text Bool
denyAll = pure True

simpleScraper :: Text -> Scraper Text Bool -> Selector -> Maybe Selector -> SiteScraper
simpleScraper sName test selectIngredients selectSteps = SiteScraper sName test scrape
  where
    scrape = (,) <$> scrapeIngredients <*> scrapeSteps
    scrapeIngredients = Scalpel.chroots selectIngredients (
      UnparsedIngredientRaw
        <$> Scalpel.text Scalpel.anySelector
      )
    scrapeSteps = case selectSteps of
      Nothing -> pure []
      Just selector -> Scalpel.chroots selector (
        UnparsedStep
          <$> Scalpel.text Scalpel.anySelector
        )

debug :: SiteScraper
debug = simpleScraper "debug" (testScrape "div") "div" (Just "div")

allrecipes :: SiteScraper
allrecipes = simpleScraper "allrecipes"
  (testScrape ("meta" @: ["content" @= "Allrecipes"]))
  ("span" @: [Scalpel.hasClass "ingredients-item-name"])
  (Just ("ul" @: [Scalpel.hasClass "instructions-section"] // "div" @: [Scalpel.hasClass "section-body"]))

-- FIXME
nytimes :: SiteScraper
nytimes = simpleScraper "nytimes"
  (testScrape ("meta" @: ["content" @= "NYT Cooking"]))
  ("ul" @: [Scalpel.hasClass "recipe-ingredients"] // "li")
  Nothing

-- FIXME
geniusKitchen1 :: SiteScraper
geniusKitchen1 = simpleScraper "geniusKitchen1"
  (testScrape ("div" @: [Scalpel.hasClass "recipe-ingredients"]))
  ("div" @: [Scalpel.hasClass "recipe-ingredients"] // "li")
  Nothing

-- FIXME
geniusKitchen2 :: SiteScraper
geniusKitchen2 = simpleScraper "geniusKitchen2"
  (testScrape ("div" @: [Scalpel.hasClass "recipe-ingredients"]))
  ("div" @: [Scalpel.hasClass "recipe-ingredients"] // "li")
  Nothing

-- FIXME
tasty1 :: SiteScraper
tasty1 = simpleScraper "tasty1"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipe-ingredients"]))
  ("div" @: [Scalpel.hasClass "tasty-recipe-ingredients"] // "li")
  Nothing

-- FIXME
tasty2 :: SiteScraper
tasty2 = simpleScraper "tasty2"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"]))
  ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"] // "li")
  Nothing

-- FIXME
tasty3 :: SiteScraper
tasty3 = simpleScraper "tasty3"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"]))
  ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"] // "p")
  Nothing

-- FIXME
foodNetwork :: SiteScraper
foodNetwork = SiteScraper "foodNetwork" denyAll $ do
  ingredients <- map UnparsedIngredientRaw . filter (not . (==) "Deselect All") <$> Scalpel.chroots ("div" @: [Scalpel.hasClass "o-Ingredients__m-Body"] // "span") (Scalpel.text Scalpel.anySelector)
  pure (ingredients, [])

-- FIXME
wprm :: SiteScraper
wprm = simpleScraper "wprm"
  (testScrape ("div" @: [Scalpel.hasClass "wprm-recipe"]))
  ("li" @: [Scalpel.hasClass "wprm-recipe-ingredient"])
  Nothing

-- FIXME
mv :: SiteScraper
mv = simpleScraper "mv"
  (testScrape ("div" @: [Scalpel.hasClass "mv-create-ingredients"]))
  ("div" @: [Scalpel.hasClass "mv-create-ingredients"] // "li")
  Nothing

-- FIXME
zl :: SiteScraper
zl = simpleScraper "zl"
  (testScrape ("ul" @: ["id" @= "zlrecipe-ingredients-list"]))
  ("ul" @: ["id" @= "zlrecipe-ingredients-list"] // "li")
  Nothing

-- FIXME
jetpack :: SiteScraper
jetpack = simpleScraper "jetpack"
  (testScrape ("div" @: [Scalpel.hasClass "jetpack-recipe"]))
  ("div" @: [Scalpel.hasClass "jetpack-recipe-ingredients"] // "li")
  Nothing

-- FIXME
eatingWell :: SiteScraper
eatingWell = simpleScraper "eatingWell"
  (testScrape ("ul" @: [Scalpel.hasClass "ingredients-section"]))
  ("ul" @: [Scalpel.hasClass "ingredients-section"] // "li")
  Nothing

-- FIXME
yummly :: SiteScraper
yummly = simpleScraper "yummly"
  denyAll
  ("li" @: [Scalpel.hasClass "IngredientLine"])
  Nothing

-- FIXME
simplyRecipes :: SiteScraper
simplyRecipes = simpleScraper "simplyrecipes"
  denyAll
  ("section" @: ["id" @= "section--ingredients_1-0"] // "li")
  Nothing

-- FIXME
ingredientLi1 :: SiteScraper
ingredientLi1 = simpleScraper "ingredientLi1"
  (testScrape ("li" @: [Scalpel.hasClass "ingredient"]))
  ("li" @: [Scalpel.hasClass "ingredient"])
  Nothing

-- FIXME
ingredientLi2 :: SiteScraper
ingredientLi2 = simpleScraper "ingredientLi2"
  (testScrape ("div" @: [Scalpel.hasClass "ingredients-section"]))
  ("div" @: [Scalpel.hasClass "ingredients-section"] // "li")
  Nothing

-- FIXME
ingredientLi3 :: SiteScraper
ingredientLi3 = simpleScraper "ingredientLi3"
  (testScrape ("li" @: ["itemprop" @= "recipeIngredient"]))
  ("li" @: ["itemprop" @= "recipeIngredient"])
  Nothing

-- FIXME
ingredientLi4 :: SiteScraper
ingredientLi4 = simpleScraper "ingredientLi4"
  (testScrape ("div" @: [Scalpel.hasClass "ingredients"]))
  ("div" @: [Scalpel.hasClass "ingredients"] // "li")
  Nothing

-- FIXME
ingredientLi5 :: SiteScraper
ingredientLi5 = simpleScraper "ingredientLi5"
  (testScrape ("div" @: [Scalpel.hasClass "listIngredient"]))
  ("div" @: [Scalpel.hasClass "listIngredient"] // "li")
  Nothing

-- FIXME
ingredientLi6 :: SiteScraper
ingredientLi6 = simpleScraper "ingredientLi6"
  (testScrape ("li" @: ["itemprop" @= "ingredients"]))
  ("li" @: ["itemprop" @= "ingredients"])
  Nothing

-- FIXME
ingredientLi7 :: SiteScraper
ingredientLi7 = simpleScraper "ingredientLi7"
  (testScrape ("ul" @: [Scalpel.hasClass "ingredients"]))
  ("ul" @: [Scalpel.hasClass "ingredients"] // "li")
  Nothing

-- FIXME
ingredientLi8 :: SiteScraper
ingredientLi8 = simpleScraper "ingredientLi8"
  (testScrape ("div" @: [Scalpel.hasClass "ingredient-text"]))
  ("div" @: [Scalpel.hasClass "ingredient-text"] // "ul" // "li")
  Nothing

-- FIXME
ingredientLi9 :: SiteScraper
ingredientLi9 = simpleScraper "ingredientLi9"
  (testScrape ("div" @: [Scalpel.hasClass "cookbook-container-ingredients"]))
  ("p" @: [Scalpel.hasClass "cookbook-ingredient-item"])
  Nothing

-- FIXME
ingredientLi10 :: SiteScraper
ingredientLi10 = simpleScraper "ingredientLi10"
  (testScrape ("table" @: [Scalpel.hasClass "ingredients-table"]))
  ("table" @: [Scalpel.hasClass "ingredients-table"] // "tr")
  Nothing

-- FIXME
ingredientLi11 :: SiteScraper
ingredientLi11 = simpleScraper "ingredientLi11"
  (testScrape ("blockquote" @: [Scalpel.hasClass "recipe-block"]))
  ("blockquote" @: [Scalpel.hasClass "recipe-block"] // "li")
  Nothing

-- FIXME
ingredientLi12 :: SiteScraper
ingredientLi12 = simpleScraper "ingredientLi12"
  (testScrape ("li" @: [Scalpel.hasClass "structured-ingredients__list-item"]))
  ("li" @: [Scalpel.hasClass "structured-ingredients__list-item"])
  Nothing

-- FIXME
ingredientLi13 :: SiteScraper
ingredientLi13 = simpleScraper "ingredientLi13"
  (testScrape ("section" @: [Scalpel.hasClass "ingredients-list"]))
  ("section" @: [Scalpel.hasClass "ingredients-list"] // "li")
  Nothing

-- FIXME
ingredientLi14 :: SiteScraper
ingredientLi14 = simpleScraper "ingredientLi14"
  (testScrape ("ul" @: [Scalpel.hasClass "page-content__ingredients-list"]))
  ("ul" @: [Scalpel.hasClass "page-content__ingredients-list"] // "li")
  Nothing

-- FIXME
delish :: SiteScraper
delish = simpleScraper "delish"
  acceptAll
  ("div" @: [Scalpel.hasClass "ingredient-item"])
  Nothing

-- FIXME
spoonacular :: SiteScraper
spoonacular = simpleScraper "spoontacular"
  denyAll
  ("div" @: [Scalpel.hasClass "spoonacular-ingredient"])
  Nothing

-- FIXME
food52 :: SiteScraper
food52 = simpleScraper "food52"
  denyAll
  ("div" @: [Scalpel.hasClass "recipe__list--ingredients"] // "li")
  Nothing

-- FIXME
-- epicurious :: SiteScraper
-- epicurious = simpleScraper "epicurious"
--   denyAll
--   ("div" @: [Scalpel.hasClass "recipe__ingredient-list"])
--   Nothing

-- FIXME
thekitchn :: SiteScraper
thekitchn = simpleScraper "thekitchn"
  denyAll
  ("ul" @: [Scalpel.hasClass "Recipe__ingredients"] // "li")
  Nothing

-- FIXME
eatwell101 :: SiteScraper
eatwell101 = simpleScraper "eatwell101"
  denyAll
  ("div" @: [Scalpel.hasClass "pf-content"] // "li")
  Nothing
