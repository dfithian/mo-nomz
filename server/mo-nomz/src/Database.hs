module Database where

import ClassyPrelude hiding (link)

import Database.PostgreSQL.Simple
  ( In(In), Only(Only), Connection, execute, executeMany, execute_, query, query_, returning
  )

import Auth (BcryptedAuthorization)
import Types
  ( Ingredient(..), Recipe(..), RecipeIngredient(..), IngredientId, RecipeId, UserId, uncurry3
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

selectIngredients :: Connection -> UserId -> [IngredientId] -> IO (Map IngredientId Ingredient)
selectIngredients conn userId ingredientIds = do
  ingredients <- case null ingredientIds of
    True -> query conn "select i.id, i.name, i.quantity, i.unit, i.active from nomz.ingredient i where i.user_id = ?" (Only userId)
    False -> query conn "select i.id, i.name, i.quantity, i.unit, i.active from nomz.ingredient i where i.user_id = ? and i.id in ?" (userId, In ingredientIds)
  pure $ foldr (\(ingredientId, name, quantity, unit, active) acc -> insertMap ingredientId (Ingredient name quantity unit active) acc) mempty ingredients

insertIngredients :: Connection -> UserId -> [Ingredient] -> IO ()
insertIngredients conn userId ingredients =
  void $ executeMany conn "insert into nomz.ingredient (user_id, name, quantity, unit, active) values (?, ?, ?, ?, ?)" $
    map (\Ingredient {..} -> (userId, ingredientName, ingredientQuantity, ingredientUnit, ingredientActive)) ingredients

mergeIngredients :: Connection -> UserId -> [IngredientId] -> Ingredient -> IO ()
mergeIngredients conn userId ingredientIds ingredient = do
  deleteIngredients conn userId ingredientIds
  insertIngredients conn userId [ingredient]

deleteIngredients :: Connection -> UserId -> [IngredientId] -> IO ()
deleteIngredients conn userId ingredientIds = do
  case null ingredientIds of
    True -> void $ execute conn "delete from nomz.ingredient where user_id = ?" (Only userId)
    False -> void $ execute conn "delete from nomz.ingredient where id in ? and user_id = ?" (In ingredientIds, userId)

selectActiveIngredients :: Connection -> UserId -> IO [RecipeIngredient]
selectActiveIngredients conn userId =
  map (uncurry3 RecipeIngredient)
    <$> query conn "select ri.name, ri.quantity, ri.unit from nomz.recipe_ingredient ri join nomz.recipe r on r.id = ri.recipe_id where r.user_id = ? and r.active" (Only userId)

selectRecipes :: Connection -> UserId -> [RecipeId] -> IO (Map RecipeId Recipe)
selectRecipes conn userId recipeIds = do
  case null recipeIds of
    True -> do
      recipes <- query conn "select r.id, r.name, r.link, r.active from nomz.recipe r where r.user_id = ?" (Only userId)
      recipeIngredients <- asMap . unionsWith (<>) . map (\(recipeId, name, quantity, unit) -> singletonMap recipeId [RecipeIngredient name quantity unit])
        <$> query conn "select r.id, ri.name, ri.quantity, ri.unit from nomz.recipe r join nomz.recipe_ingredient ri on r.id = ri.recipe_id where r.user_id = ?" (Only userId)
      pure $ foldr (\(recipeId, name, link, active) -> insertMap recipeId (Recipe name link (findWithDefault [] recipeId recipeIngredients) active)) mempty recipes
    False -> do
      recipes <- query conn "select r.id, r.name, r.link, r.active from nomz.recipe r where r.user_id = ? and r.id in ?" (userId, In recipeIds)
      recipeIngredients <- asMap . unionsWith (<>) . map (\(recipeId, name, quantity, unit) -> singletonMap recipeId [RecipeIngredient name quantity unit])
        <$> query conn "select r.id, ri.name, ri.quantity, ri.unit from nomz.recipe r join nomz.recipe_ingredient ri on r.id = ri.recipe_id where r.user_id = ? and r.id in ?" (userId, In recipeIds)
      pure $ foldr (\(recipeId, name, link, active) -> insertMap recipeId (Recipe name link (findWithDefault [] recipeId recipeIngredients) active)) mempty recipes

insertRecipe :: Connection -> UserId -> Recipe -> IO ()
insertRecipe conn userId Recipe {..} = do
  [(Only (recipeId :: RecipeId))] <- returning conn "insert into nomz.recipe (user_id, name, link, active) values (?, ?, ?, ?) returning id" [(userId, recipeName, recipeLink, recipeActive)]
  void $ executeMany conn "insert into nomz.recipe_ingredient (recipe_id, name, quantity, unit) values (?, ?, ?, ?)" $
    map (\RecipeIngredient {..} -> (recipeId, recipeIngredientName, recipeIngredientQuantity, recipeIngredientUnit)) recipeIngredients

updateRecipe :: Connection -> UserId -> RecipeId -> Bool -> IO ()
updateRecipe conn userId recipeId active = do
  void $ execute conn "update nomz.recipe set active = ? where id = ? and user_id = ?" (active, recipeId, userId)

deleteRecipes :: Connection -> UserId -> [RecipeId] -> IO ()
deleteRecipes conn userId recipeIds = do
  case null recipeIds of
    True -> do
      void $ execute_ conn "delete from nomz.recipe_ingredient"
      void $ execute conn "delete from nomz.recipe where user_id = ?" (Only userId)
    False -> do
      void $ execute conn "delete from nomz.recipe_ingredient where recipe_id in ?" (Only (In recipeIds))
      void $ execute conn "delete from nomz.recipe where user_id = ? and id in ?" (userId, In recipeIds)
