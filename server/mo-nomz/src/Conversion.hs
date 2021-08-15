module Conversion where

import ClassyPrelude

import Data.Monoid (Sum(..))

import API.Types (ReadableGroceryItem(..), ReadableIngredient(..), ReadableRecipe(..))
import Combinable (Constant(..), Combinable)
import Types
  ( GroceryItem(..), Ingredient(..), OrderedGroceryItem(..), OrderedIngredient(..), Quantity(..)
  , ReadableFraction(..), ReadableQuantity(..), ReadableUnit(..), Recipe(..), Unit(..), IngredientId
  , cup, gram, liter, milligram, milliliter, ounce, pinch, tablespoon, teaspoon
  )
import qualified Combinable as C
import qualified Data.Map as Map

data UnitHierarchy
  = UnitHierarchyEnd (Unit, Quantity)
  | UnitHierarchyMid (Unit, Quantity) (Unit, Quantity)
  deriving (Eq, Ord, Show)

conversionTable :: Map Unit UnitHierarchy
conversionTable = mapFromList
  [ (ounce, UnitHierarchyEnd (cup, 8))
  , (cup, UnitHierarchyMid (tablespoon, 16) (ounce, 0.125))
  , (tablespoon, UnitHierarchyMid (teaspoon, 3) (cup, 0.0625))
  , (teaspoon, UnitHierarchyMid (pinch, 4) (tablespoon, Quantity $ 1 / 3))
  , (pinch, UnitHierarchyEnd (tablespoon, 0.25))

  , (liter, UnitHierarchyEnd (milliliter, 1000))
  , (gram, UnitHierarchyEnd (milligram, 1000))
  ]

getAllConversions :: Unit -> Map Unit Quantity
getAllConversions = snd . go mempty
  where
    go oldVisited next =
      let visited = asSet $ insertSet next oldVisited
      in case (member next oldVisited, lookup next conversionTable) of
        (True, _) -> (oldVisited, mempty)
        (_, Nothing) -> (visited, mempty)
        (_, Just (UnitHierarchyEnd (x, q))) ->
          let (newVisited, newConversions) = go visited x
          in (newVisited, insertMap x q $ map ((*) q) newConversions)
        (_, Just (UnitHierarchyMid (x1, q1) (x2, q2))) ->
          let (newVisited1, newConversions1) = go visited x1
              (newVisited2, newConversions2) = go newVisited1 x2
          in (newVisited2, mapFromList [(x1, q1), (x2, q2)] <> map ((*) q1) newConversions1 <> (map ((*) q2) newConversions2))

knownUnitOrdering :: Map Unit Int
knownUnitOrdering = mapFromList $ zip [ounce, cup, tablespoon, teaspoon, pinch] [1..]

unitOrdering :: Unit -> Unit -> Ordering
unitOrdering x y = case (lookup x knownUnitOrdering, lookup y knownUnitOrdering) of
  (Just a, Just b) -> compare a b
  (Just _, Nothing) -> LT
  (Nothing, Just _) -> GT
  (Nothing, Nothing) -> compare x y

wrapSidecar :: (Quantity, a) -> (Sum Quantity, Constant a)
wrapSidecar (x, y) = (Sum x, Constant y)

unwrapSidecar :: (Sum Quantity, Constant a) -> (Quantity, a)
unwrapSidecar (Sum x, Constant y) = (x, y)

combineQuantities :: Map Unit (Sum Quantity, Constant a) -> Map Unit (Sum Quantity, Constant a)
combineQuantities = foldr go mempty . reverse . sortBy (\(x, _) (y, _) -> unitOrdering x y) . mapToList
  where
    go (nextUnit, (nextQuantity, nextSidecar)) acc = case lookup nextUnit acc of
      Just (existingQuantity, existingSidecar) -> asMap $ insertMap nextUnit (nextQuantity <> existingQuantity, nextSidecar <> existingSidecar) acc
      Nothing ->
        let allConversions = getAllConversions nextUnit
            allConversionsKeys = asSet . setFromList . keys $ allConversions
            existingKeys = asSet . setFromList . keys $ acc
            overlappingKeys = headMay . sortBy unitOrdering . setToList . intersect allConversionsKeys $ existingKeys
        in case overlappingKeys of
          Nothing -> insertMap nextUnit (nextQuantity, nextSidecar) acc
          Just existingKey -> insertWith (<>) existingKey (Sum $ getSum nextQuantity * findWithDefault 1 existingKey allConversions, nextSidecar) acc

combineItems :: Combinable a => [a] -> [a]
combineItems =
  mconcat
    . map (\(name, everythingElse) -> map (\(unit, (quantity, sidecar)) -> C.orig (name, quantity, unit, sidecar)) $ mapToList everythingElse)
    . mapToList
    . map (map unwrapSidecar . combineQuantities . foldr (uncurry (insertWith (<>))) mempty . map (second wrapSidecar))
    . unionsWith (<>)
    . map ( \x ->
        let (name, quantity, unit, sidecar) = C.mk x
        in asMap $ singletonMap name [(unit, (quantity, sidecar))]
      )

readableQuantityPrecision :: Double
readableQuantityPrecision = 0.01

readableQuantities :: [((Double, Double), (Int, Int))]
readableQuantities =
  [ ((quarter - readableQuantityPrecision, quarter + readableQuantityPrecision), (1, 4))
  , ((third - readableQuantityPrecision, third + readableQuantityPrecision), (1, 3))
  , ((half - readableQuantityPrecision, half + readableQuantityPrecision), (1, 2))
  , ((twoThird - readableQuantityPrecision, twoThird + readableQuantityPrecision), (2, 3))
  , ((threeQuarter - readableQuantityPrecision, threeQuarter + readableQuantityPrecision), (3, 4))
  ]
  where
    quarter = 0.25
    third = 1 / 3
    half = 0.5
    twoThird = 2 / 3
    threeQuarter = 0.75

splitQuantity :: Quantity -> Maybe (Int, Double)
splitQuantity = \case
  QuantityMissing -> Nothing
  Quantity q -> case abs (fromIntegral (round q :: Int) - q) < readableQuantityPrecision of
    True -> Just (round q, 0.0)
    False -> let whole = truncate q in Just (whole, q - fromIntegral whole)

mkReadableQuantity :: Quantity -> ReadableQuantity
mkReadableQuantity q = case splitQuantity q of
  Nothing -> ReadableQuantity Nothing Nothing
  Just (whole, decimal) ->
    case (whole == 0, find (\((lo, hi), _) -> lo <= decimal && decimal <= hi) readableQuantities) of
      (False, Just (_, (numerator, denominator))) -> ReadableQuantity (Just whole) (Just (ReadableFraction numerator denominator))
      (True, Just (_, (numerator, denominator))) -> ReadableQuantity Nothing (Just (ReadableFraction numerator denominator))
      (False, Nothing) -> ReadableQuantity (Just whole) Nothing
      (True, Nothing) -> ReadableQuantity Nothing Nothing

mkQuantity :: ReadableQuantity -> Quantity
mkQuantity q =
  let raw = case q of
        ReadableQuantity (Just whole) (Just (ReadableFraction numerator denominator)) -> fromIntegral whole + (fromIntegral numerator / fromIntegral denominator)
        ReadableQuantity Nothing (Just (ReadableFraction numerator denominator)) -> fromIntegral numerator / fromIntegral denominator
        ReadableQuantity (Just whole) Nothing -> fromIntegral whole
        ReadableQuantity Nothing Nothing -> 0
  in if raw == 0 then QuantityMissing else Quantity raw

mkReadableUnit :: Unit -> Maybe ReadableUnit
mkReadableUnit = \case
  Unit x -> Just (ReadableUnit x)
  UnitMissing -> Nothing

mkUnit :: Maybe ReadableUnit -> Unit
mkUnit = \case
  Just (ReadableUnit x) | x /= "" -> Unit x
  _ -> UnitMissing

mkReadableGroceryItem :: OrderedGroceryItem -> ReadableGroceryItem
mkReadableGroceryItem OrderedGroceryItem {..} =
  let GroceryItem {..} = orderedGroceryItemItem
  in ReadableGroceryItem
    { readableGroceryItemName = groceryItemName
    , readableGroceryItemQuantity = mkReadableQuantity groceryItemQuantity
    , readableGroceryItemUnit = mkReadableUnit groceryItemUnit
    , readableGroceryItemActive = groceryItemActive
    , readableGroceryItemOrder = orderedGroceryItemOrder
    }

mkGroceryItem :: ReadableGroceryItem -> GroceryItem
mkGroceryItem ReadableGroceryItem {..} = GroceryItem
  { groceryItemName = readableGroceryItemName
  , groceryItemQuantity = mkQuantity readableGroceryItemQuantity
  , groceryItemUnit = mkUnit readableGroceryItemUnit
  , groceryItemActive = readableGroceryItemActive
  }

mkReadableIngredient :: OrderedIngredient -> ReadableIngredient
mkReadableIngredient OrderedIngredient {..} = ReadableIngredient
  { readableIngredientName = ingredientName
  , readableIngredientQuantity = mkReadableQuantity ingredientQuantity
  , readableIngredientUnit = mkReadableUnit ingredientUnit
  , readableIngredientOrder = orderedIngredientOrder
  }
  where
    Ingredient {..} = orderedIngredientIngredient

mkOrderedIngredient :: ReadableIngredient -> OrderedIngredient
mkOrderedIngredient ReadableIngredient {..} = OrderedIngredient
  { orderedIngredientIngredient = Ingredient
    { ingredientName = readableIngredientName
    , ingredientQuantity = mkQuantity readableIngredientQuantity
    , ingredientUnit = mkUnit readableIngredientUnit
    }
  , orderedIngredientOrder = readableIngredientOrder
  }

mkReadableRecipe :: Map IngredientId OrderedIngredient -> Recipe -> ReadableRecipe
mkReadableRecipe ingredients Recipe {..} = ReadableRecipe
  { readableRecipeName = recipeName
  , readableRecipeLink = recipeLink
  , readableRecipeActive = recipeActive
  , readableRecipeRating = recipeRating
  , readableRecipeNotes = recipeNotes
  , readableRecipeIngredients = mkReadableIngredient <$> Map.elems ingredients
  , readableRecipeIngredientsV2 = mkReadableIngredient <$> ingredients
  }
