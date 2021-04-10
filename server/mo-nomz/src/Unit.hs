module Unit where

import ClassyPrelude

import Data.Monoid (Sum(Sum), getSum)

import Types
  ( Quantity(..), ReadableFraction(..), ReadableQuantity(..), Unit(..), cup, ounce, pinch
  , tablespoon, teaspoon
  )

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

combineQuantities :: Semigroup a => Map Unit (a, Quantity) -> Map Unit (a, ReadableQuantity)
combineQuantities = map (second (mkReadableQuantity . getSum)) . foldr go mempty . reverse . sortBy (\(x, _) (y, _) -> unitOrdering x y) . mapToList
  where
    go (nextUnit, (nextExtra, nextQuantity)) acc = case lookup nextUnit acc of
      Just (existingExtra, existingQuantity) -> asMap $ insertMap nextUnit (nextExtra <> existingExtra, Sum nextQuantity <> existingQuantity) acc
      Nothing ->
        let allConversions = getAllConversions nextUnit
            allConversionsKeys = asSet . setFromList . keys $ allConversions
            existingKeys = asSet . setFromList . keys $ acc
            overlappingKeys = headMay . sortBy unitOrdering . setToList . intersect allConversionsKeys $ existingKeys
        in case overlappingKeys of
          Nothing -> insertMap nextUnit (nextExtra, Sum nextQuantity) acc
          Just existingKey -> insertWith (<>) existingKey (nextExtra, Sum (nextQuantity * findWithDefault 1 existingKey allConversions)) acc

readableQuantityPrecision :: Double
readableQuantityPrecision = 0.001

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

mkReadableQuantity :: Quantity -> ReadableQuantity
mkReadableQuantity (Quantity q) =
  let whole = truncate q
      decimal = q - fromIntegral whole
  in case (whole == 0, find (\((lo, hi), _) -> lo <= decimal && decimal <= hi) readableQuantities) of
    (False, Just (_, (numerator, denominator))) -> ReadableQuantity (Just whole) (Just (ReadableFraction numerator denominator))
    (True, Just (_, (numerator, denominator))) -> ReadableQuantity Nothing (Just (ReadableFraction numerator denominator))
    (False, Nothing) -> ReadableQuantity (Just whole) Nothing
    (True, Nothing) -> ReadableQuantity Nothing Nothing

mkQuantity :: ReadableQuantity -> Quantity
mkQuantity = \case
  ReadableQuantity (Just whole) (Just (ReadableFraction numerator denominator)) -> Quantity $ fromIntegral whole + (fromIntegral numerator / fromIntegral denominator)
  ReadableQuantity Nothing (Just (ReadableFraction numerator denominator)) -> Quantity $ fromIntegral numerator / fromIntegral denominator
  ReadableQuantity (Just whole) Nothing -> Quantity $ fromIntegral whole
  ReadableQuantity Nothing Nothing -> Quantity 0
