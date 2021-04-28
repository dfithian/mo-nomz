module Database where

import ClassyPrelude hiding (link)

import Database.PostgreSQL.Simple
  ( In(In), Only(Only), Connection, execute, executeMany, fromOnly, query, query_, returning
  )
import qualified Data.Map as Map

import Auth (BcryptedAuthorization)
import Conversion (combineIngredients)
import Types
  ( GroceryItem(..), Ingredient(..), Recipe(..), GroceryItemId, IngredientId, RecipeId, UserId
  , groceryItemToIngredient, ingredientToGroceryItem, ingredientToGroceryItem'
  )

data DatabaseException = DatabaseException Text
  deriving (Eq, Show)

instance Exception DatabaseException

health :: Connection -> IO ()
health conn = do
  [Only (1 :: Int)] <- query_ conn "select 1"
  pure ()

insertToken :: Connection -> BcryptedAuthorization -> IO UserId
insertToken conn token = do
  [(Only userId)] <- returning conn "insert into nomz.user (token, is_valid) values (?, ?) returning id" [(token, True)]
  pure userId

fetchToken :: Connection -> UserId -> IO (Maybe BcryptedAuthorization)
fetchToken conn userId = do
  query conn "select token from nomz.user where id = ? and is_valid" (Only userId) >>= \case
    [(Only token)] -> pure token
    _ -> pure Nothing

selectGroceryItems :: Connection -> UserId -> [GroceryItemId] -> IO (Map GroceryItemId GroceryItem)
selectGroceryItems conn userId groceryItemIds = do
  groceryItems <- case null groceryItemIds of
    True -> query conn "select id, name, quantity, unit, active from nomz.grocery_item where user_id = ?" (Only userId)
    False -> query conn "select id, name, quantity, unit, active from nomz.grocery_item where user_id = ? and id in ?" (userId, In groceryItemIds)
  pure $ foldr (\(groceryItemId, name, quantity, unit, active) acc -> insertMap groceryItemId (GroceryItem name quantity unit active) acc) mempty groceryItems

insertGroceryItems :: Connection -> UserId -> [GroceryItem] -> IO [GroceryItemId]
insertGroceryItems conn userId groceryItems =
  map (map fromOnly)
    . returning conn "insert into nomz.grocery_item (user_id, name, quantity, unit, active) values (?, ?, ?, ?, ?) returning id"
    . map (\GroceryItem {..} -> (userId, groceryItemName, groceryItemQuantity, groceryItemUnit, groceryItemActive))
    $ groceryItems

mergeGroceryItems :: Connection -> UserId -> [GroceryItemId] -> GroceryItem -> IO GroceryItemId
mergeGroceryItems conn userId oldGroceryItemIds newGroceryItem = do
  [newGroceryItemId] <- insertGroceryItems conn userId [newGroceryItem]
  mergeIngredientGroceryItemIds conn userId oldGroceryItemIds newGroceryItemId
  deleteGroceryItems conn userId oldGroceryItemIds
  pure newGroceryItemId

automergeGroceryItems :: Connection -> UserId -> IO ()
automergeGroceryItems c userId = do
  (activeGroceryItems, inactiveGroceryItems) <- Map.partition groceryItemActive <$> selectGroceryItems c userId []
  let remap (groceryItemId, GroceryItem {..}) = insertWith (<>) (groceryItemName, groceryItemUnit) (asSet $ singletonSet groceryItemId)
      activeGroceryItemIdsByNameAndUnit = asMap . foldr remap mempty . mapToList $ activeGroceryItems
      inactiveGroceryItemIdsByNameAndUnit = asMap . foldr remap mempty . mapToList $ inactiveGroceryItems
      combinedActiveGroceryItems = map ingredientToGroceryItem . combineIngredients . map groceryItemToIngredient . Map.elems $ activeGroceryItems
      combinedInactiveGroceryItems = map (ingredientToGroceryItem' False) . combineIngredients . map groceryItemToIngredient . Map.elems $ inactiveGroceryItems
      (newActiveGroceryItems, oldActiveGroceryItemIds) = unzip $ flip mapMaybe combinedActiveGroceryItems $ \x@GroceryItem {..} ->
        case lookup (groceryItemName, groceryItemUnit) activeGroceryItemIdsByNameAndUnit of
          Just xs | length xs >= 2 -> Just (x, setToList xs)
          _ -> Nothing
      (newInactiveGroceryItems, oldInactiveGroceryItemIds) = unzip $ flip mapMaybe combinedInactiveGroceryItems $ \x@GroceryItem {..} ->
        case lookup (groceryItemName, groceryItemUnit) inactiveGroceryItemIdsByNameAndUnit of
          Just xs | length xs >= 2 -> Just (x, setToList xs)
          _ -> Nothing
  newGroceryItemIds <- insertGroceryItems c userId (newActiveGroceryItems <> newInactiveGroceryItems)
  for_ (zip newGroceryItemIds (oldActiveGroceryItemIds <> oldInactiveGroceryItemIds)) $ \(newGroceryItemId, oldGroceryItemIds) ->
    mergeIngredientGroceryItemIds c userId oldGroceryItemIds newGroceryItemId
  deleteGroceryItems c userId (mconcat oldActiveGroceryItemIds <> mconcat oldInactiveGroceryItemIds)

unmergeGroceryItems :: Connection -> UserId -> [IngredientId] -> IO ()
unmergeGroceryItems conn userId oldIngredientIds =
  case null oldIngredientIds of
    True -> pure mempty
    False -> do
      oldGroceryItemIds <- map fromOnly
        <$> query conn "select distinct(grocery_id) from nomz.ingredient where user_id = ? and id in ? and grocery_id is not null" (userId, In oldIngredientIds)
      void $ execute conn "update nomz.ingredient set grocery_id = null where user_id = ? and id in ?" (userId, In oldIngredientIds)
      (unmergedIngredientIds, newGroceryItems) <- unzip . map (\(ingredientId, name, quantity, unit, active) -> (ingredientId, GroceryItem name quantity unit active))
        <$> query conn "select i.id, g.name, i.quantity, i.unit, g.active from nomz.ingredient i join nomz.grocery_item g on g.id = i.grocery_id where i.user_id = ? and i.grocery_id in ?" (userId, In oldGroceryItemIds)

      newGroceryItemIds <- insertGroceryItems conn userId newGroceryItems
      for_ (zip newGroceryItemIds unmergedIngredientIds) $ \(newGroceryItemId, unmergedIngredientId) ->
        unmergeIngredientGroceryItemIds conn userId unmergedIngredientId newGroceryItemId
      deleteGroceryItems conn userId oldGroceryItemIds
      automergeGroceryItems conn userId

mergeIngredientGroceryItemIds :: Connection -> UserId -> [GroceryItemId] -> GroceryItemId -> IO ()
mergeIngredientGroceryItemIds conn userId groceryItemIds groceryItemId =
  void $ execute conn "update nomz.ingredient set grocery_id = ? where user_id = ? and grocery_id in ?" (groceryItemId, userId, In groceryItemIds)

unmergeIngredientGroceryItemIds :: Connection -> UserId -> IngredientId -> GroceryItemId -> IO ()
unmergeIngredientGroceryItemIds conn userId ingredientId groceryItemId =
  void $ execute conn "update nomz.ingredient set grocery_id = ? where user_id = ? and id = ?" (groceryItemId, userId, ingredientId)

deleteGroceryItems :: Connection -> UserId -> [GroceryItemId] -> IO ()
deleteGroceryItems conn userId groceryItemIds =
  case null groceryItemIds of
    True -> pure ()
    False -> do
      void $ execute conn "update nomz.ingredient set grocery_id = null where grocery_id in ? and user_id = ? and recipe_id is not null" (In groceryItemIds, userId)
      void $ execute conn "delete from nomz.ingredient where grocery_id in ? and user_id = ? and recipe_id is null" (In groceryItemIds, userId)
      void $ execute conn "delete from nomz.grocery_item where id in ? and user_id = ?" (In groceryItemIds, userId)

deactivateEverything :: Connection -> UserId -> IO ()
deactivateEverything conn userId = do
  void $ execute conn "update nomz.ingredient set grocery_id = null where user_id = ? and recipe_id is not null" (Only userId)
  void $ execute conn "delete from nomz.ingredient where user_id = ? and recipe_id is null" (Only userId)
  void $ execute conn "update nomz.recipe set active = false where user_id = ?" (Only userId)
  void $ execute conn "delete from nomz.grocery_item where user_id = ?" (Only userId)

insertGroceryItemIngredients :: Connection -> UserId -> [(GroceryItemId, Ingredient)] -> IO [IngredientId]
insertGroceryItemIngredients conn userId ingredients =
  map (map fromOnly)
    . returning conn "insert into nomz.ingredient (grocery_id, user_id, name, quantity, unit) values (?, ?, ?, ?, ?) returning id"
    . map (\(groceryItemId, Ingredient {..}) -> (groceryItemId, userId, ingredientName, ingredientQuantity, ingredientUnit))
    $ ingredients

selectIngredientsByRecipeId :: Connection -> UserId -> RecipeId -> IO [(IngredientId, Ingredient)]
selectIngredientsByRecipeId conn userId recipeId =
  map (\(ingredientId, name, quantity, unit) -> (ingredientId, Ingredient name quantity unit))
    <$> query conn "select id, name, quantity, unit from nomz.ingredient where user_id = ? and recipe_id = ?" (userId, recipeId)

selectRecipeIngredientIds :: Connection -> UserId -> [RecipeId] -> IO [IngredientId]
selectRecipeIngredientIds conn userId recipeIds =
  case null recipeIds of
    True -> pure []
    False -> map fromOnly
      <$> query conn "select id from nomz.ingredient where user_id = ? and recipe_id in ?" (userId, In recipeIds)

selectRecipes :: Connection -> UserId -> [RecipeId] -> IO (Map RecipeId Recipe)
selectRecipes conn userId recipeIds = do
  case null recipeIds of
    True -> do
      recipes <- query conn "select r.id, r.name, r.link, r.active from nomz.recipe r where r.user_id = ?" (Only userId)
      pure . mapFromList . map (\(recipeId, name, link, active) -> (recipeId, Recipe name link active)) $ recipes
    False -> do
      recipes <- query conn "select r.id, r.name, r.link, r.active from nomz.recipe r where r.user_id = ? and r.id in ?" (userId, In recipeIds)
      pure . mapFromList . map (\(recipeId, name, link, active) -> (recipeId, Recipe name link active)) $ recipes

insertRecipe :: Connection -> UserId -> Recipe -> [(GroceryItemId, Ingredient)] -> IO RecipeId
insertRecipe conn userId Recipe {..} ingredients = do
  [(Only (recipeId :: RecipeId))] <- returning conn "insert into nomz.recipe (user_id, name, link, active) values (?, ?, ?, ?) returning id" [(userId, recipeName, recipeLink, recipeActive)]
  void $ executeMany conn "insert into nomz.ingredient (recipe_id, grocery_id, user_id, name, quantity, unit) values (?, ?, ?, ?, ?, ?)" $
    map (\(groceryItemId, Ingredient {..}) -> (recipeId, groceryItemId, userId, ingredientName, ingredientQuantity, ingredientUnit)) ingredients
  pure recipeId

deactivateRecipe :: Connection -> UserId -> RecipeId -> IO ()
deactivateRecipe conn userId recipeId = do
  ingredientIds <- selectRecipeIngredientIds conn userId [recipeId]
  unless (null ingredientIds) $ do
    void $ execute conn "update nomz.ingredient set grocery_id = null where user_id = ? and id in ?" (userId, In ingredientIds)
  void $ execute conn "update nomz.recipe set active = false where user_id = ? and id = ?" (userId, recipeId)

activateRecipe :: Connection -> UserId -> RecipeId -> [(GroceryItemId, IngredientId)] -> IO ()
activateRecipe conn userId recipeId groceryAndIngredientIds = do
  for_ groceryAndIngredientIds $ \(groceryItemId, ingredientId) ->
    void $ execute conn "update nomz.ingredient set grocery_id = ? where user_id = ? and id = ?" (groceryItemId, userId, ingredientId)
  void $ execute conn "update nomz.recipe set active = true where user_id = ? and id = ?" (userId, recipeId)

deleteRecipes :: Connection -> UserId -> [RecipeId] -> IO ()
deleteRecipes conn userId recipeIds = do
  case null recipeIds of
    True -> pure ()
    False -> void $ execute conn "delete from nomz.recipe where user_id = ? and id in ?" (userId, In recipeIds)
