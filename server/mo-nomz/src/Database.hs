module Database where

import ClassyPrelude hiding (link)

import Database.PostgreSQL.Simple
  ( In(In), Only(Only), Connection, execute, executeMany, query, query_, returning, withTransaction
  )

import Types (Ingredient(..), Recipe(..), User(..), RecipeId, UserId, Username, uncurry3)

insertUser :: Connection -> Username -> IO UserId
insertUser conn username = do
  [Only userId] <- returning conn "insert into nomz.user (username) values (?) returning (id)" [Only username]
  pure userId

fetchUserIdByUsername :: Connection -> Username -> IO (Maybe UserId)
fetchUserIdByUsername conn username = do
  query conn "select u.id from nomz.user u where u.username = ?" (Only username) >>= \case
    [Only userId] -> pure $ Just userId
    _ -> pure Nothing

fetchUserExists :: Connection -> UserId -> IO Bool
fetchUserExists conn userId = do
  query conn "select 1 from nomz.user u where u.id = ?" (Only userId) >>= \case
    [Only (1 :: Int)] -> pure True
    _ -> pure False

selectUsersByUsername :: Connection -> [Username] -> IO (Map UserId User)
selectUsersByUsername conn usernames = do
  users <- case null usernames of
    True -> query_ conn "select u.id, u.username from nomz.user u"
    False -> query conn "select u.id, u.username from nomz.user u where u.username in ?" (Only (In usernames))
  pure . mapFromList . map (second User) $ users

selectIngredients :: Connection -> UserId -> [RecipeId] -> IO [Ingredient]
selectIngredients conn userId recipeIds = do
  ingredients <- case null recipeIds of
    True -> query conn "select i.name, i.quantity, i.unit from nomz.ingredient i join nomz.recipe r on r.id = i.recipe_id where r.user_id = ?" (Only userId)
    False -> query conn "select i.name, i.quantity, i.unit from nomz.ingredient i join nomz.recipe r on r.id = i.recipe_id where r.user_id = ? and r.id in ?" (userId, In recipeIds)
  pure $ uncurry3 Ingredient <$> ingredients

selectRecipes :: Connection -> UserId -> [RecipeId] -> IO (Map RecipeId Recipe)
selectRecipes conn userId recipeIds = do
  recipes <- case null recipeIds of
    True -> query conn "select r.id, r.name, r.link from nomz.recipe r where r.user_id = ?" (Only userId)
    False -> query conn "select r.id, r.name, r.link from nomz.recipe r where r.user_id = ? and r.id in ?" (userId, In recipeIds)
  ingredients <- asMap . foldr (\(recipeId, name, quantity, unit) acc -> insertWith (<>) recipeId [Ingredient name quantity unit] acc) mempty
    <$> query_ conn "select r.id, i.name, i.quantity, i.unit from nomz.recipe r join nomz.ingredient i on i.recipe_id = r.id"
  pure $ foldr (\(recipeId, name, link) acc -> insertMap recipeId (Recipe name (findWithDefault mempty recipeId ingredients) link) acc) mempty recipes

deleteRecipe :: Connection -> UserId -> RecipeId -> IO ()
deleteRecipe conn userId recipeId = withTransaction conn $ do
  void $ execute conn "delete from nomz.ingredient i using nomz.recipe r where i.recipe_id = r.id and r.user_id = ? and r.id = ?" (userId, recipeId)
  void $ execute conn "delete from nomz.recipe where user_id = ? and id = ?" (userId, recipeId)

insertIngredients :: Connection -> RecipeId -> [Ingredient] -> IO ()
insertIngredients conn recipeId ingredients =
  void $ executeMany conn "insert into nomz.ingredient (recipe_id, name, quantity, unit) values (?, ?, ?, ?)" $
    map (\Ingredient {..} -> (recipeId, ingredientName, ingredientQuantity, ingredientUnit)) ingredients

insertRecipe :: Connection -> UserId -> Recipe -> IO RecipeId
insertRecipe conn userId Recipe {..} = withTransaction conn $ do
  [Only recipeId] <- returning conn "insert into nomz.recipe (user_id, name, link) values (?, ?, ?) returning (id)" [(userId, recipeName, recipeLink)]
  insertIngredients conn recipeId recipeIngredients
  pure recipeId

updateRecipe :: Connection -> RecipeId -> Recipe -> IO ()
updateRecipe conn recipeId Recipe {..} = withTransaction conn $ do
  void $ execute conn "update nomz.recipe set name = ?, link = ? where id = ?" (recipeName, recipeLink, recipeId)
  void $ execute conn "delete from nomz.ingredient where recipe_id = ?" (Only recipeId)
  insertIngredients conn recipeId recipeIngredients
