module Gen where

import ClassyPrelude
import Data.CaseInsensitive (CI)
import Test.QuickCheck (Gen, arbitrary, elements, oneof)
import qualified Data.CaseInsensitive as CI

import Types

arbitraryAlphaNum :: Gen Char
arbitraryAlphaNum = elements $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']

arbitraryAlphaNumStr :: Gen Text
arbitraryAlphaNumStr = do
  n <- arbitrary
  pack <$> replicateM (abs n + 10) arbitraryAlphaNum

arbitraryCi :: Gen (CI Text)
arbitraryCi = CI.mk <$> arbitraryAlphaNumStr

arbitraryIngredientName :: Gen IngredientName
arbitraryIngredientName = IngredientName <$> arbitraryCi

arbitraryRecipeName :: Gen RecipeName
arbitraryRecipeName = RecipeName <$> arbitraryAlphaNumStr

trunc :: Int -> Double -> Double
trunc n x = (fromIntegral (floor (x * t) :: Int)) / t
  where t = 10^n

arbitraryDouble :: Gen Double
arbitraryDouble = trunc 5 . abs <$> arbitrary

arbitraryQuantity :: Gen Quantity
arbitraryQuantity = oneof
  [ pure QuantityMissing
  , Quantity <$> arbitraryDouble
  ]

arbitraryUnit :: Gen Unit
arbitraryUnit = oneof
  [ pure UnitMissing
  , Unit <$> arbitraryCi
  ]

arbitraryGroceryItem :: Gen GroceryItem
arbitraryGroceryItem = GroceryItem
  <$> arbitraryIngredientName
  <*> arbitraryQuantity
  <*> arbitraryUnit
  <*> arbitrary

arbitraryOrderedGroceryItem :: Gen OrderedGroceryItem
arbitraryOrderedGroceryItem = OrderedGroceryItem
  <$> arbitraryGroceryItem
  <*> arbitrary

arbitraryIngredient :: Gen Ingredient
arbitraryIngredient = Ingredient
  <$> arbitraryIngredientName
  <*> arbitraryQuantity
  <*> arbitraryUnit

arbitraryRecipe :: Gen Recipe
arbitraryRecipe = Recipe
  <$> arbitraryRecipeName
  <*> pure Nothing
  <*> pure True
  <*> pure 0
  <*> pure ""
