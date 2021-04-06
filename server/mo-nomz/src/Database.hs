module Database where

import ClassyPrelude hiding (link)

import Database.PostgreSQL.Simple
  ( In(In), Only(Only), Connection, execute, executeMany, query, query_, returning, withTransaction
  )

import Types (Ingredient(..), Recipe(..), RecipeId, RecipeLink, RecipeName, uncurry3)

selectIngredients :: Connection -> [RecipeId] -> IO [Ingredient]
selectIngredients conn recipeIds = do
  ingredients <- case null recipeIds of
    True -> query_ conn "select i.name, i.quantity, i.unit from nomz.ingredient i"
    False -> query conn "select i.name, i.quantity, i.unit from nomz.ingredient i where i.recipe_id in ?" (Only (In recipeIds))
  pure $ uncurry3 Ingredient <$> ingredients

selectRecipeIngredients :: Connection -> [RecipeId] -> IO (Map RecipeId Recipe)
selectRecipeIngredients conn recipeIds = do
  recipes <- case null recipeIds of
    True -> query_ conn "select r.id, r.name, r.link from nomz.recipe r"
    False -> query conn "select r.id, r.name, r.link from nomz.recipe r where r.id in ?" (Only (In recipeIds))
  ingredients <- asMap . foldr (\(recipeId, name, quantity, unit) acc -> insertWith (<>) recipeId [Ingredient name quantity unit] acc) mempty
    <$> query_ conn "select r.id, i.name, i.quantity, i.unit from nomz.recipe r join nomz.ingredient i on i.recipe_id = r.id"
  pure $ foldr (\(recipeId, name, link) acc -> insertMap recipeId (Recipe name (findWithDefault mempty recipeId ingredients) link) acc) mempty recipes

deleteRecipe :: Connection -> RecipeId -> IO ()
deleteRecipe conn recipeId = withTransaction conn $ do
  void $ execute conn "delete from nomz.ingredient where recipe_id = ?" (Only recipeId)
  void $ execute conn "delete from nomz.recipe where id = ?" (Only recipeId)

insertRecipe :: Connection -> RecipeName -> Maybe RecipeLink -> [Ingredient] -> IO RecipeId
insertRecipe conn name link ingredients = withTransaction conn $ do
  [Only recipeId] <- returning conn "insert into nomz.recipe (name, link) values (?, ?) returning (id)" [(name, link)]
  void $ executeMany conn "insert into nomz.ingredient (recipe_id, name, quantity, unit) values (?, ?, ?, ?)" $
    map (\Ingredient {..} -> (recipeId, ingredientName, ingredientQuantity, ingredientUnit)) ingredients
  pure recipeId
