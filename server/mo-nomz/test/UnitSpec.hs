module UnitSpec where

import ClassyPrelude

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (forAll, shuffle)

import Types
  ( ReadableFraction(..), ReadableQuantity(..), Unit(..), cup, ounce, pinch, tablespoon, teaspoon
  )

import Unit

allUnits :: [Unit]
allUnits = [ounce, cup, tablespoon, teaspoon, pinch, Unit "none", Unit "whole"]

spec :: Spec
spec = describe "Unit" $ do
  it "gets all conversions for ounce" $
    getAllConversions ounce `shouldBe` mapFromList [(ounce, 1), (cup, 8), (tablespoon, 128), (teaspoon, 384), (pinch, 1536)]
  it "orders correctly" $ forAll (shuffle allUnits) $ \xs ->
    sortBy unitOrdering xs `shouldBe` allUnits
  it "combines quantities - cup equivalent to ounce" $
    combineQuantities (mapFromList [(cup, ([1], 8)), (ounce, ([2], 1))]) `shouldBe` mapFromList [(ounce, ([1, 2], ReadableQuantity (Just 2) Nothing))]
  it "combines quantities - cup less than ounce" $
    combineQuantities (mapFromList [(cup, ([1], 4)), (ounce, ([2], 1))]) `shouldBe` mapFromList [(ounce, ([1, 2], ReadableQuantity (Just 1) (Just (ReadableFraction 1 2))))]
  it "combines quantities - teaspoons to tablespoons to cups" $
    combineQuantities (mapFromList [(teaspoon, ([1], 2)), (tablespoon, ([2], 2)), (cup, ([3], 4))]) `shouldBe` mapFromList [(cup, ([1, 2, 3], ReadableQuantity (Just 4) (Just (ReadableFraction 1 4))))]
  it "retains quantities not in known conversions" $
    combineQuantities (mapFromList [(ounce, ([1], 1)), (Unit "whole", ([2], 1))]) `shouldBe` mapFromList [(ounce, ([1], ReadableQuantity (Just 1) Nothing)), (Unit "whole", ([2], ReadableQuantity (Just 1) Nothing))]
