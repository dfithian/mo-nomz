module UnitSpec where

import ClassyPrelude

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (forAll, shuffle)

import Types (Unit(..))

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
    combineQuantities (mapFromList [(cup, 8), (ounce, 1)]) `shouldBe` mapFromList [(ounce, 2)]
  it "combines quantities - cup less than ounce" $
    combineQuantities (mapFromList [(cup, 4), (ounce, 1)]) `shouldBe` mapFromList [(ounce, 1.5)]
  it "combines quantities - teaspoons to tablespoons to cups" $
    combineQuantities (mapFromList [(teaspoon, 2), (tablespoon, 2), (cup, 4)]) `shouldBe` mapFromList [(cup, 4.25)]
  it "retains quantities not in known conversions" $
    combineQuantities (mapFromList [(ounce, 1), (Unit "whole", 1)]) `shouldBe` mapFromList [(ounce, 1), (Unit "whole", 1)]
