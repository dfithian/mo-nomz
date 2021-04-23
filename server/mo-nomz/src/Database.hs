module Database where

import ClassyPrelude hiding (link)

import Database.PostgreSQL.Simple
  ( In(In), Only(Only), Connection, execute, executeMany, fromOnly, query, query_, returning
  )

import Auth (BcryptedAuthorization)
import Types (GroceryItem(..), Ingredient(..), Recipe(..), GroceryItemId, RecipeId, UserId, uncurry3)

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

mergeGroceryItems :: Connection -> UserId -> [GroceryItemId] -> GroceryItem -> IO ()
mergeGroceryItems conn userId groceryItemIds groceryItem = do
  [groceryItemId] <- insertGroceryItems conn userId [groceryItem]
  updateIngredientGroceryItemIds conn userId groceryItemIds groceryItemId
  deleteGroceryItems conn userId groceryItemIds

updateIngredientGroceryItemIds :: Connection -> UserId -> [GroceryItemId] -> GroceryItemId -> IO ()
updateIngredientGroceryItemIds conn userId groceryItemIds groceryItemId =
  void $ execute conn "update nomz.ingredient set grocery_id = ? where user_id = ? and grocery_id in ?" (groceryItemId, userId, In groceryItemIds)

deleteGroceryItems :: Connection -> UserId -> [GroceryItemId] -> IO ()
deleteGroceryItems conn userId groceryItemIds = do
  case null groceryItemIds of
    True -> pure ()
    False -> void $ execute conn "delete from nomz.grocery_item where id in ? and user_id = ?" (In groceryItemIds, userId)

clearGroceryItems :: Connection -> UserId -> IO ()
clearGroceryItems conn userId =
  void $ execute conn "delete from nomz.grocery_item where user_id = ?" (Only userId)

insertRecipeIngredients :: Connection -> UserId -> RecipeId -> [Ingredient] -> IO ()
insertRecipeIngredients conn userId recipeId ingredients =
  void $ executeMany conn "insert into nomz.ingredient (recipe_id, user_id, name, quantity, unit) values (?, ?, ?, ?, ?)" $
    map (\Ingredient {..} -> (recipeId, userId, ingredientName, ingredientQuantity, ingredientUnit)) ingredients

insertGroceryItemIngredients :: Connection -> UserId -> [(GroceryItemId, Ingredient)] -> IO ()
insertGroceryItemIngredients conn userId ingredients =
  void $ executeMany conn "insert into nomz.ingredient (grocery_id, user_id, name, quantity, unit) values (?, ?, ?, ?, ?)" $
    map (\(groceryItemId, Ingredient {..}) -> (groceryItemId, userId, ingredientName, ingredientQuantity, ingredientUnit)) ingredients

selectActiveIngredients :: Connection -> UserId -> IO [Ingredient]
selectActiveIngredients conn userId =
  map (uncurry3 Ingredient)
    <$> query conn "select i.name, i.quantity, i.unit from nomz.ingredient i left join nomz.recipe r on r.id = i.recipe_id where i.user_id = ? and coalesce(r.active, true)" (Only userId)

selectRecipes :: Connection -> UserId -> [RecipeId] -> IO (Map RecipeId Recipe)
selectRecipes conn userId recipeIds = do
  case null recipeIds of
    True -> do
      recipes <- query conn "select r.id, r.name, r.link, r.active from nomz.recipe r where r.user_id = ?" (Only userId)
      pure . mapFromList . map (\(recipeId, name, link, active) -> (recipeId, Recipe name link active)) $ recipes
    False -> do
      recipes <- query conn "select r.id, r.name, r.link, r.active from nomz.recipe r where r.user_id = ? and r.id in ?" (userId, In recipeIds)
      pure . mapFromList . map (\(recipeId, name, link, active) -> (recipeId, Recipe name link active)) $ recipes

insertRecipe :: Connection -> UserId -> Recipe -> [Ingredient] -> IO ()
insertRecipe conn userId Recipe {..} ingredients = do
  [(Only (recipeId :: RecipeId))] <- returning conn "insert into nomz.recipe (user_id, name, link, active) values (?, ?, ?, ?) returning id" [(userId, recipeName, recipeLink, recipeActive)]
  insertRecipeIngredients conn userId recipeId ingredients

updateRecipes :: Connection -> UserId -> [RecipeId] -> Bool -> IO ()
updateRecipes conn userId recipeIds active =
  case null recipeIds of
    True -> void $ execute conn "update nomz.recipe set active = ? where user_id = ?" (active, userId)
    False -> void $ execute conn "update nomz.recipe set active = ? where id in ? and user_id = ?" (active, In recipeIds, userId)

deleteRecipes :: Connection -> UserId -> [RecipeId] -> IO ()
deleteRecipes conn userId recipeIds = do
  case null recipeIds of
    True -> pure ()
    False -> void $ execute conn "delete from nomz.recipe where user_id = ? and id in ?" (userId, In recipeIds)
