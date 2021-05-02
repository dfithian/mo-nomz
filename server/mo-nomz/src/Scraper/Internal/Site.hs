module Scraper.Internal.Site where

import ClassyPrelude
import Text.HTML.Scalpel ((//), (@:), (@=), Scraper, Selector)
import qualified Data.HashMap.Strict as HashMap
import qualified Text.HTML.Scalpel as Scalpel

import Scraper.Internal.Types (SiteName(..), SiteScraper(..), UnparsedIngredient(..))

siteScrapers :: HashMap SiteName SiteScraper
siteScrapers = mapFromList
  [ ("allrecipes.com", allrecipes)

  , ("cooking.nytimes.com", nytimes)

  , ("food.com", geniusKitchen1)
  , ("geniuskitchen.com", geniusKitchen1)
  , ("tasteofhome.com", geniusKitchen2)

  , ("bettycrocker.com", bettyCrocker)

  , ("rachlmansfield.com", tasty1)
  , ("cookieandkate.com", tasty1)
  , ("simpleveganblog.com", tasty1)
  , ("lexiscleankitchen.com", tasty1)
  , ("eatyourselfskinny.com", tasty1)
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

  , ("melskitchencafe.com", mv)
  , ("glutenfreecuppatea.co.uk", mv)

  , ("localmilkblog.com", zl)

  , ("smittenkitchen.com", jetpack)

  , ("eatingwell.com", eatingWell)
  , ("bhg.com", eatingWell)

  , ("yummly.com", yummly)

  , ("pillsbury.com", ingredientLi1)
  , ("epicurious.com", ingredientLi1)
  , ("tasty.co", ingredientLi1)
  , ("seriouseats.com", ingredientLi1)
  , ("simplyrecipes.com", ingredientLi1)
  , ("lazycatkitchen.com", ingredientLi2)
  , ("deliciouslyella.com", ingredientLi3)
  , ("cookingandcooking.com", ingredientLi5)
  , ("damndelicious.net", ingredientLi6)
  , ("hemsleyandhemsley.com", ingredientLi6)
  , ("slenderkitchen.com", ingredientLi7)
  , ("everydayannie.com", ingredientLi8)
  , ("notwithoutsalt.com", ingredientLi9)
  , ("chefspencil.com", ingredientLi10)
  , ("sweetandsavorymeals.com", ingredientLi11)

  , ("delish.com", delish)
  , ("thepioneerwoman.com", delish)

  , ("spoonacular.com", spoonacular)

  , ("food52.com", food52)

  , ("thekitchn.com", thekitchn)
  ]

-- |Get all site scrapers, ordered by most popular first.
allSiteScrapers :: [SiteScraper]
allSiteScrapers = map unsafeHead . reverse . sortOn length  . groupBy ((==) `on` siteScraperName) . sortOn siteScraperName . HashMap.elems $ siteScrapers

testScrape :: Selector -> Scraper Text Bool
testScrape test = not . null <$> Scalpel.html test

acceptAll :: Scraper Text Bool
acceptAll = pure True

denyAll :: Scraper Text Bool
denyAll = pure False

simpleScraper :: Text -> Scraper Text Bool -> Selector -> SiteScraper
simpleScraper sName test select = SiteScraper sName test scrape
  where
    scrape = Scalpel.chroots select (
      UnparsedIngredientRaw
        <$> Scalpel.text Scalpel.anySelector
      )

debug :: SiteScraper
debug = simpleScraper "debug" (testScrape "div") "div"

allrecipes :: SiteScraper
allrecipes = simpleScraper "allrecipes"
  (testScrape ("meta" @: ["content" @= "Allrecipes"]))
  ("span" @: [Scalpel.hasClass "ingredients-item-name"])

nytimes :: SiteScraper
nytimes = simpleScraper "nytimes"
  (testScrape ("meta" @: ["content" @= "NYT Cooking"]))
  ("ul" @: [Scalpel.hasClass "recipe-ingredients"] // "li")

geniusKitchen1 :: SiteScraper
geniusKitchen1 = simpleScraper "geniusKitchen1"
  (testScrape ("div" @: [Scalpel.hasClass "recipe-ingredients"]))
  ("div" @: [Scalpel.hasClass "recipe-ingredients"] // "li")

geniusKitchen2 :: SiteScraper
geniusKitchen2 = simpleScraper "geniusKitchen2"
  (testScrape ("div" @: [Scalpel.hasClass "recipe-ingredients"]))
  ("div" @: [Scalpel.hasClass "recipe-ingredients"] // "li")

bettyCrocker :: SiteScraper
bettyCrocker = simpleScraper "bettyCrocker"
  denyAll
  ("div" @: [Scalpel.hasClass "recipePartIngredient"])

tasty1 :: SiteScraper
tasty1 = simpleScraper "tasty1"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipe-ingredients"]))
  ("div" @: [Scalpel.hasClass "tasty-recipe-ingredients"] // "li")

tasty2 :: SiteScraper
tasty2 = simpleScraper "tasty2"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"]))
  ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"] // "li")

tasty3 :: SiteScraper
tasty3 = simpleScraper "tasty3"
  (testScrape ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"]))
  ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"] // "p")

foodNetwork :: SiteScraper
foodNetwork = SiteScraper "foodNetwork"
  denyAll
  (map UnparsedIngredientRaw . filter (not . (==) "Deselect All") <$> Scalpel.chroots ("div" @: [Scalpel.hasClass "o-Ingredients__m-Body"] // "span") (Scalpel.text Scalpel.anySelector))

wprm :: SiteScraper
wprm = simpleScraper "wprm"
  (testScrape ("div" @: [Scalpel.hasClass "wprm-recipe"]))
  ("li" @: [Scalpel.hasClass "wprm-recipe-ingredient"])

mv :: SiteScraper
mv = simpleScraper "mv"
  (testScrape ("div" @: [Scalpel.hasClass "mv-create-ingredients"]))
  ("div" @: [Scalpel.hasClass "mv-create-ingredients"] // "li")

zl :: SiteScraper
zl = simpleScraper "zl"
  (testScrape ("ul" @: ["id" @= "zlrecipe-ingredients-list"]))
  ("ul" @: ["id" @= "zlrecipe-ingredients-list"] // "li")

jetpack :: SiteScraper
jetpack = simpleScraper "jetpack"
  (testScrape ("div" @: [Scalpel.hasClass "jetpack-recipe"]))
  ("div" @: [Scalpel.hasClass "jetpack-recipe-ingredients"] // "li")

eatingWell :: SiteScraper
eatingWell = simpleScraper "eatingWell"
  (testScrape ("ul" @: [Scalpel.hasClass "ingredients-section"]))
  ("ul" @: [Scalpel.hasClass "ingredients-section"] // "li")

yummly :: SiteScraper
yummly = simpleScraper "yummly"
  denyAll
  ("li" @: [Scalpel.hasClass "IngredientLine"])

ingredientLi1 :: SiteScraper
ingredientLi1 = simpleScraper "ingredientLi1"
  (testScrape ("li" @: [Scalpel.hasClass "ingredient"]))
  ("li" @: [Scalpel.hasClass "ingredient"])

ingredientLi2 :: SiteScraper
ingredientLi2 = simpleScraper "ingredientLi2"
  (testScrape ("div" @: [Scalpel.hasClass "ingredients-list"]))
  ("div" @: [Scalpel.hasClass "ingredients-list"] // "li")

ingredientLi3 :: SiteScraper
ingredientLi3 = simpleScraper "ingredientLi3"
  (testScrape ("li" @: ["itemprop" @= "recipeIngredient"]))
  ("li" @: ["itemprop" @= "recipeIngredient"])

ingredientLi4 :: SiteScraper
ingredientLi4 = simpleScraper "ingredientLi4"
  (testScrape ("div" @: [Scalpel.hasClass "ingredients"]))
  ("div" @: [Scalpel.hasClass "ingredients"] // "li")

ingredientLi5 :: SiteScraper
ingredientLi5 = simpleScraper "ingredientLi5"
  (testScrape ("div" @: [Scalpel.hasClass "listIngredient"]))
  ("div" @: [Scalpel.hasClass "listIngredient"] // "li")

ingredientLi6 :: SiteScraper
ingredientLi6 = simpleScraper "ingredientLi6"
  (testScrape ("li" @: ["itemprop" @= "ingredients"]))
  ("li" @: ["itemprop" @= "ingredients"])

ingredientLi7 :: SiteScraper
ingredientLi7 = simpleScraper "ingredientLi7"
  (testScrape ("ul" @: [Scalpel.hasClass "ingredients"]))
  ("ul" @: [Scalpel.hasClass "ingredients"] // "li")

ingredientLi8 :: SiteScraper
ingredientLi8 = simpleScraper "ingredientLi8"
  (testScrape ("div" @: [Scalpel.hasClass "ingredient-text"]))
  ("div" @: [Scalpel.hasClass "ingredient-text"] // "ul" // "li")

ingredientLi9 :: SiteScraper
ingredientLi9 = simpleScraper "ingredientLi9"
  (testScrape ("div" @: [Scalpel.hasClass "cookbook-container-ingredients"]))
  ("p" @: [Scalpel.hasClass "cookbook-ingredient-item"])

ingredientLi10 :: SiteScraper
ingredientLi10 = simpleScraper "ingredientLi10"
  (testScrape ("table" @: [Scalpel.hasClass "ingredients-table"]))
  ("table" @: [Scalpel.hasClass "ingredients-table"] // "tr")

ingredientLi11 :: SiteScraper
ingredientLi11 = simpleScraper "ingredientLi11"
  (testScrape ("div" @: [Scalpel.hasClass "product-recipe__ingredients"]))
  ("div" @: [Scalpel.hasClass "product-recipe__ingredients"] // "li")

delish :: SiteScraper
delish = simpleScraper "delish"
  acceptAll
  ("div" @: [Scalpel.hasClass "ingredient-item"])

spoonacular :: SiteScraper
spoonacular = simpleScraper "spoontacular"
  denyAll
  ("div" @: [Scalpel.hasClass "spoonacular-ingredient"])

food52 :: SiteScraper
food52 = simpleScraper "food52"
  denyAll
  ("div" @: [Scalpel.hasClass "recipe__list--ingredients"] // "li")

thekitchn :: SiteScraper
thekitchn = simpleScraper "thekitchn"
  denyAll
  ("ul" @: [Scalpel.hasClass "Recipe__ingredients"] // "li")
