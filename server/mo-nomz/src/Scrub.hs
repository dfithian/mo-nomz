module Scrub where

import ClassyPrelude

import Data.CaseInsensitive (CI)

import Types
  ( Ingredient(..), Quantity(..), RawIngredient(..), RawQuantity(..), RawUnit(..), Unit(..), box
  , cup, ounce, pinch, pound, splash, sprinkle, tablespoon, teaspoon
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
  , ("box", box)
  , ("boxes", box)
  , ("pound", pound)
  , ("pounds", pound)
  , ("splash", splash)
  , ("splashes", splash)
  , ("sprinkle", sprinkle)
  , ("sprinkles", sprinkle)
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
  , ingredientActive = True
  }
