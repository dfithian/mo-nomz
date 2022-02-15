module Combinable where

import Prelude

import Types
  ( GroceryItem(..), Ingredient(..), OrderedGroceryItem(..), IngredientName, Quantity, Unit
  )

newtype Constant a = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Semigroup (Constant a) where
  x <> _ = x

class Combinable a where
  type Sidecar a :: *
  mk :: a -> (IngredientName, Quantity, Unit, Sidecar a)
  orig :: (IngredientName, Quantity, Unit, Sidecar a) -> a

instance Combinable Ingredient where
  type Sidecar Ingredient = ()
  mk Ingredient {..} = (ingredientName, ingredientQuantity, ingredientUnit, ())
  orig (ingredientName, ingredientQuantity, ingredientUnit, ()) = Ingredient {..}

instance Combinable GroceryItem where
  type Sidecar GroceryItem = Bool
  mk GroceryItem {..} = (groceryItemName, groceryItemQuantity, groceryItemUnit, groceryItemActive)
  orig (groceryItemName, groceryItemQuantity, groceryItemUnit, groceryItemActive) = GroceryItem {..}

instance Combinable OrderedGroceryItem where
  type Sidecar OrderedGroceryItem = (Bool, Int)
  mk OrderedGroceryItem {..} =
    let (name, quantity, unit, active) = mk orderedGroceryItemItem
    in (name, quantity, unit, (active, orderedGroceryItemOrder))
  orig (name, quantity, unit, (active, orderedGroceryItemOrder)) =
    let orderedGroceryItemItem = orig (name, quantity, unit, active)
    in OrderedGroceryItem {..}
