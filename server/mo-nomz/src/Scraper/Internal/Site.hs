module Scraper.Internal.Site where

import ClassyPrelude
import Data.Text (replace)
import Network.URI (URI, uriAuthority, uriRegName)
import Text.HTML.Scalpel ((//), (@:), (@=), Scraper, Selector)
import qualified Data.HashMap.Strict as HashMap
import qualified Text.HTML.Scalpel as Scalpel

import Scraper.Internal.Types
  ( SiteName(..), SiteScraper(..), UnparsedIngredient(..), UnparsedQuantity(..)
  , UnparsedQuantityUnit(..), UnparsedUnit(..)
  )

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

  , ("melskitchencafe.com", mv)
  , ("glutenfreecuppatea.co.uk", mv)

  , ("localmilkblog.com", zl)

  , ("smittenkitchen.com", jetpack)

  , ("eatingwell.com", eatingWell)

  , ("yummly.com", yummly)

  , ("pillsbury.com", ingredientLi1)
  , ("epicurious.com", ingredientLi1)
  , ("tasty.co", ingredientLi1)
  , ("lazycatkitchen.com", ingredientLi2)
  , ("deliciouslyella.com", ingredientLi3)
  , ("cookingandcooking.com", ingredientLi5)
  , ("damndelicious.net", ingredientLi6)
  , ("hemsleyandhemsley.com", ingredientLi6)

  , ("delish.com", delish)

  , ("spoonacular.com", spoonacular)
  ]

-- |Take the top 10 most popular site scrapers as the defaults.
defaultScrapers :: [Scraper Text [UnparsedIngredient]]
defaultScrapers = map siteScraperRun . take 10 . map unsafeHead . reverse . sortOn length  . groupBy ((==) `on` siteScraperName) . sortOn siteScraperName . HashMap.elems $ siteScrapers

getSiteScrapers :: URI -> Either [Scraper Text [UnparsedIngredient]] (Scraper Text [UnparsedIngredient])
getSiteScrapers uri =
  let domainMay = SiteName . replace "www." "" . pack . uriRegName <$> uriAuthority uri
  in case flip lookup siteScrapers =<< domainMay of
    Nothing -> Left defaultScrapers
    Just scraper -> Right $ siteScraperRun scraper

simpleScraper :: Text -> Selector -> SiteScraper
simpleScraper sName select = SiteScraper sName scrape
  where
    scrape = Scalpel.chroots select (
      UnparsedIngredientRaw
        <$> Scalpel.text Scalpel.anySelector
      )

quantityScraper :: Text -> Selector -> Selector -> Selector -> SiteScraper
quantityScraper sName select quantity nameAndUnit = SiteScraper sName scrape
  where
    scrape = Scalpel.chroots select (
      UnparsedIngredientStructured1
        <$> (UnparsedQuantity <$> Scalpel.text quantity)
        <*> Scalpel.text nameAndUnit
     )

quantityUnitScraper :: Text -> Selector -> Selector -> Selector -> SiteScraper
quantityUnitScraper sName select quantityUnit name = SiteScraper sName scrape
  where
    scrape = Scalpel.chroots select (
      UnparsedIngredientStructured3
        <$> (UnparsedQuantityUnit <$> Scalpel.text quantityUnit)
        <*> Scalpel.text name
      )

quantityAndUnitScraper :: Text -> Selector -> Selector -> Selector -> Selector -> SiteScraper
quantityAndUnitScraper sName select quantity unit name = SiteScraper sName scrape
  where
    scrape = Scalpel.chroots select (
      UnparsedIngredientStructured2
        <$> (UnparsedQuantity <$> Scalpel.text quantity)
        <*> (UnparsedUnit <$> Scalpel.text unit)
        <*> Scalpel.text name
      )

debug :: SiteScraper
debug = simpleScraper "debug" "div"

allrecipes :: SiteScraper
allrecipes = simpleScraper "allrecipes" ("span" @: [Scalpel.hasClass "ingredients-item-name"])

nytimes :: SiteScraper
nytimes = simpleScraper "nytimes" ("ul" @: [Scalpel.hasClass "recipe-ingredients"] // "li")

geniusKitchen1 :: SiteScraper
geniusKitchen1 = quantityScraper "geniusKitchen1"
  ("div" @: [Scalpel.hasClass "recipe-ingredients"] // "li")
  ("div" @: [Scalpel.hasClass "recipe-ingredients__ingredient-quantity"])
  ("div" @: [Scalpel.hasClass "recipe-ingredients__ingredient-parts"])

geniusKitchen2 :: SiteScraper
geniusKitchen2 = simpleScraper "geniusKitchen2" ("div" @: [Scalpel.hasClass "recipe-ingredients"] // "li")

bettyCrocker :: SiteScraper
bettyCrocker = simpleScraper "bettyCrocker" ("div" @: [Scalpel.hasClass "recipePartIngredient"])

tasty1 :: SiteScraper
tasty1 = simpleScraper "tasty1" ("div" @: [Scalpel.hasClass "tasty-recipe-ingredients"] // "li")

tasty2 :: SiteScraper
tasty2 = simpleScraper "tasty2" ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"] // "li")

tasty3 :: SiteScraper
tasty3 = simpleScraper "tasty3" ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"] // "p")

foodNetwork :: SiteScraper
foodNetwork = SiteScraper "foodNetwork" (map UnparsedIngredientRaw . filter (not . (==) "Deselect All") <$> Scalpel.chroots ("div" @: [Scalpel.hasClass "o-Ingredients__m-Body"] // "span") (Scalpel.text Scalpel.anySelector))

wprm :: SiteScraper
wprm = simpleScraper "wprm" ("li" @: [Scalpel.hasClass "wprm-recipe-ingredient"])

mv :: SiteScraper
mv = simpleScraper "mv" ("div" @: [Scalpel.hasClass "mv-create-ingredients"] // "li")

zl :: SiteScraper
zl = simpleScraper "zl" ("ul" @: ["id" @= "zlrecipe-ingredients-list"] // "li")

jetpack :: SiteScraper
jetpack = simpleScraper "jetpack" ("div" @: [Scalpel.hasClass "jetpack-recipe-ingredients"] // "li")

eatingWell :: SiteScraper
eatingWell = simpleScraper "eatingWell" ("ul" @: [Scalpel.hasClass "ingredients-section"] // "li")

yummly :: SiteScraper
yummly = simpleScraper "yummly" ("li" @: [Scalpel.hasClass "IngredientLine"])

ingredientLi1 :: SiteScraper
ingredientLi1 = simpleScraper "ingredientLi1" ("li" @: [Scalpel.hasClass "ingredient"])

ingredientLi2 :: SiteScraper
ingredientLi2 = simpleScraper "ingredientLi2" ("div" @: [Scalpel.hasClass "ingredients-list"] // "li")

ingredientLi3 :: SiteScraper
ingredientLi3 = simpleScraper "ingredientLi3" ("li" @: ["itemprop" @= "recipeIngredient"])

ingredientLi4 :: SiteScraper
ingredientLi4 = simpleScraper "ingredientLi4" ("div" @: [Scalpel.hasClass "ingredients"] // "li")

ingredientLi5 :: SiteScraper
ingredientLi5 = simpleScraper "ingredientLi5" ("div" @: [Scalpel.hasClass "listIngredient"] // "li")

ingredientLi6 :: SiteScraper
ingredientLi6 = simpleScraper "ingredientLi6" ("li" @: ["itemprop" @= "ingredients"])

delish :: SiteScraper
delish = simpleScraper "delish" ("div" @: [Scalpel.hasClass "ingredient-item"])

spoonacular :: SiteScraper
spoonacular = simpleScraper "spoontacular" ("div" @: [Scalpel.hasClass "spoonacular-ingredient"])
