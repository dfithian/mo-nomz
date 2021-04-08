module Unit where

import ClassyPrelude

import Types (Quantity(..), Unit(..), cup, ounce, pinch, tablespoon, teaspoon)

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

combineQuantities :: Map Unit Quantity -> Map Unit Quantity
combineQuantities = foldr go mempty . reverse . sortBy (\(x, _) (y, _) -> unitOrdering x y) . mapToList
  where
    go (nextUnit, nextQuantity) acc = case lookup nextUnit acc of
      Just existingQuantity -> insertMap nextUnit (nextQuantity + existingQuantity) acc
      Nothing ->
        let allConversions = getAllConversions nextUnit
            allConversionsKeys = asSet . setFromList . keys $ allConversions
            existingKeys = asSet . setFromList . keys $ acc
            overlappingKeys = headMay . sortBy unitOrdering . setToList . intersect allConversionsKeys $ existingKeys
        in case overlappingKeys of
          Nothing -> insertMap nextUnit nextQuantity acc
          Just existingKey -> insertWith (+) existingKey (nextQuantity * findWithDefault 1 existingKey allConversions) acc
