module Scrub where

import ClassyPrelude
import Data.Char (isAlpha)
import Data.Text (replace, splitOn)

import Data.CaseInsensitive (CI)

import Types
  ( Ingredient(..), Quantity(..), RawIngredient(..), RawQuantity(..), RawUnit(..), RecipeName(..)
  , Unit(..), cup, ounce, pinch, tablespoon, teaspoon
  )

unitAliasTable :: Map (CI Text) Unit
unitAliasTable = mapFromList
  [ ("ounce", ounce)
  , ("ounces", ounce)
  , ("oz", ounce)
  , ("cup", cup)
  , ("cups", cup)
  , ("tablespoon", tablespoon)
  , ("tablespoons", tablespoon)
  , ("tbsp", tablespoon)
  , ("teaspoon", teaspoon)
  , ("teaspoons", teaspoon)
  , ("tsp", teaspoon)
  , ("pinch", pinch)
  , ("pinches", pinch)
  ]

quantityAliasTable :: Map (CI Text) Quantity
quantityAliasTable = mapFromList
  [ ("half dozen", Quantity 6)
  , ("dozen", Quantity 12)
  , ("quarter", Quantity 0.25)
  , ("third", Quantity $ 1 / 3)
  , ("half", Quantity 0.5)
  , ("one", Quantity 1)
  , ("two", Quantity 2)
  , ("three", Quantity 3)
  , ("four", Quantity 4)
  , ("five", Quantity 5)
  , ("six", Quantity 6)
  , ("seven", Quantity 7)
  , ("eight", Quantity 8)
  , ("nine", Quantity 9)
  , ("ten", Quantity 10)
  , ("eleven", Quantity 11)
  , ("twelve", Quantity 12)
  ]

scrubUnit :: RawUnit -> Unit
scrubUnit = \case
  RawUnitWord x -> findWithDefault (Unit x) x unitAliasTable
  RawUnitMissing -> Unit "whole"

scrubQuantity :: RawQuantity -> Quantity
scrubQuantity = \case
  RawQuantityPure q -> q
  RawQuantityWord w -> findWithDefault 1 w quantityAliasTable
  RawQuantityMissing -> 1

scrubIngredient :: RawIngredient -> Ingredient
scrubIngredient RawIngredient {..} = Ingredient
  { ingredientName = rawIngredientName
  , ingredientQuantity = scrubQuantity rawIngredientQuantity
  , ingredientUnit = scrubUnit rawIngredientUnit
  }

scrubRecipeName :: RecipeName -> RecipeName
scrubRecipeName (RecipeName n) =
  let subdomain = take 15 . intercalate "." . filter (not . null) . dropEnd 1 . splitOn "." . takeWhile (not . (==) '/') . replace "www" "" . replace "http://" "" $ n
      name = take 15 . fromMaybe "" . lastMay . sortOn length . filter (all (\c -> isAlpha c || c == '-')) . drop 1 . splitOn "/" $ n
  in case (null subdomain, null name) of
    (True, True) -> RecipeName n
    (False, True) -> RecipeName subdomain
    (True, False) -> RecipeName name
    (False, False) -> RecipeName $ subdomain <> " " <> name
