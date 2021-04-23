module Database where

import ClassyPrelude hiding (link)

import Database.PostgreSQL.Simple
  ( In(In), Only(Only), Connection, execute, executeMany, query, query_, returning
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
    True -> query conn "select id, name, quantity, unit, active from nomz.grocery_item i where user_id = ?" (Only userId)
    False -> query conn "select id, name, quantity, unit, active from nomz.grocery_item i where user_id = ? and id in ?" (userId, In groceryItemIds)
  pure $ foldr (\(groceryItemId, name, quantity, unit, active) acc -> insertMap groceryItemId (GroceryItem name quantity unit active) acc) mempty groceryItems

insertGroceryItems :: Connection -> UserId -> [GroceryItem] -> IO ()
insertGroceryItems conn userId groceryItems =
  void $ executeMany conn "insert into nomz.grocery_item (user_id, name, quantity, unit, active) values (?, ?, ?, ?, ?)" $
    map (\GroceryItem {..} -> (userId, groceryItemName, groceryItemQuantity, groceryItemUnit, groceryItemActive)) groceryItems

mergeGroceryItems :: Connection -> UserId -> [GroceryItemId] -> GroceryItem -> IO ()
mergeGroceryItems conn userId groceryItemIds groceryItem = do
  deleteGroceryItems conn userId groceryItemIds
  insertGroceryItems conn userId [groceryItem]

deleteGroceryItems :: Connection -> UserId -> [GroceryItemId] -> IO ()
deleteGroceryItems conn userId groceryItemIds = do
  case null groceryItemIds of
    True -> void $ execute conn "delete from nomz.grocery_item where user_id = ?" (Only userId)
    False -> void $ execute conn "delete from nomz.grocery_item where id in ? and user_id = ?" (In groceryItemIds, userId)

insertIngredients :: Connection -> UserId -> [Ingredient] -> IO ()
insertIngredients conn userId ingredients =
  void $ executeMany conn "insert into nomz.ingredient (user_id, name, quantity, unit) values (?, ?, ?, ?)" $
    map (\Ingredient {..} -> (userId, ingredientName, ingredientQuantity, ingredientUnit)) ingredients

selectActiveIngredients :: Connection -> UserId -> IO [Ingredient]
selectActiveIngredients conn userId = do
  recipeIngredients <- map (uncurry3 Ingredient)
    <$> query conn "select ri.name, ri.quantity, ri.unit from nomz.recipe_ingredient ri join nomz.recipe r on r.id = ri.recipe_id where r.user_id = ? and r.active" (Only userId)
  ingredients <- map (uncurry3 Ingredient)
    <$> query conn "select name, quantity, unit from nomz.ingredient where user_id = ?" (Only userId)
  pure $ recipeIngredients <> ingredients

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
  void $ executeMany conn "insert into nomz.recipe_ingredient (recipe_id, name, quantity, unit) values (?, ?, ?, ?)" $
    map (\Ingredient {..} -> (recipeId, ingredientName, ingredientQuantity, ingredientUnit)) ingredients

updateRecipe :: Connection -> UserId -> RecipeId -> Bool -> IO ()
updateRecipe conn userId recipeId active = do
  void $ execute conn "update nomz.recipe set active = ? where id = ? and user_id = ?" (active, recipeId, userId)

deleteRecipes :: Connection -> UserId -> [RecipeId] -> IO ()
deleteRecipes conn userId recipeIds = do
  case null recipeIds of
    True -> pure ()
    False -> do
      void $ execute conn "delete from nomz.recipe_ingredient where recipe_id in ?" (Only (In recipeIds))
      void $ execute conn "delete from nomz.recipe where user_id = ? and id in ?" (userId, In recipeIds)
