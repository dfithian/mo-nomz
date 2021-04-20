module Scraper.Internal.Site where

import ClassyPrelude
import Data.Text (replace)
import Text.HTML.Scalpel (Scraper, (@:), (//))
import Network.URI (URI, uriRegName, uriAuthority)
import qualified Text.HTML.Scalpel as Scalpel

import Scraper.Internal.Types (SiteName(..), defaultScraper)

siteScrapers :: Map SiteName (Scraper Text Text)
siteScrapers = mapFromList
  [ ("allrecipes.com", allrecipes)
  , ("food.com", food)
  , ("geniuskitchen.com", food)
  , ("pillsbury.com", pillsbury)
  , ("tasteofhome.com", tasteOfHome)
  , ("rachlmansfield.com", rachelMansfield)
  , ("foodnetwork.com", foodNetwork)
  , ("sallysbakingaddiction.com", sallysBaking)
  , ("cafedelites.com", cafeDelites)
  ]

getSiteScraper :: URI -> Scraper Text Text
getSiteScraper uri =
  let domainMay = SiteName . replace "www." "" . pack . uriRegName <$> uriAuthority uri
  in case domainMay of
    Nothing -> defaultScraper
    Just domain -> findWithDefault defaultScraper domain siteScrapers

allrecipes :: Scraper Text Text
allrecipes = unlines <$> Scalpel.chroots ("span" @: [Scalpel.hasClass "ingredients-item-name"]) (Scalpel.text Scalpel.anySelector)

food :: Scraper Text Text
food = unlines <$> Scalpel.chroots ("div" @: [Scalpel.hasClass "recipe-ingredients"] // "li") (Scalpel.text Scalpel.anySelector)

pillsbury :: Scraper Text Text
pillsbury = unlines <$> Scalpel.chroots ("div" @: [Scalpel.hasClass "recipePartIngredient"]) (Scalpel.text Scalpel.anySelector)

tasteOfHome :: Scraper Text Text
tasteOfHome = unlines <$> Scalpel.chroots ("div" @: [Scalpel.hasClass "recipe-ingredients"] // "li") (Scalpel.text Scalpel.anySelector)

rachelMansfield :: Scraper Text Text
rachelMansfield = unlines <$> Scalpel.chroots ("div" @: [Scalpel.hasClass "tasty-recipe-ingredients"] // "li") (Scalpel.text Scalpel.anySelector)

foodNetwork :: Scraper Text Text
foodNetwork = unlines . filter (not . (==) "Deselect All") <$> Scalpel.chroots ("div" @: [Scalpel.hasClass "o-Ingredients__m-Body"] // "span") (Scalpel.text Scalpel.anySelector)

sallysBaking :: Scraper Text Text
sallysBaking = unlines <$> Scalpel.chroots ("div" @: [Scalpel.hasClass "tasty-recipes-ingredients"] // "li") (Scalpel.text Scalpel.anySelector)

cafeDelites :: Scraper Text Text
cafeDelites = unlines <$> Scalpel.chroots ("li" @: [Scalpel.hasClass "wprm-recipe-ingredient"]) (Scalpel.text Scalpel.anySelector)
