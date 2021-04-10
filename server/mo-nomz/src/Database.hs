module Database where

import ClassyPrelude hiding (link)

import Database.PostgreSQL.Simple
  ( In(In), Only(Only), Connection, execute, executeMany, query, returning, withTransaction
  )

import Types (Ingredient(..), IngredientId, UserId, Username)

newtype DatabaseException = DatabaseException Text
  deriving (Eq, Show)

instance Exception DatabaseException

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

selectIngredients :: Connection -> UserId -> [IngredientId] -> IO (Map IngredientId Ingredient)
selectIngredients conn userId ingredientIds = do
  ingredients <- case null ingredientIds of
    True -> query conn "select i.id, i.name, i.quantity, i.unit from nomz.ingredient i where i.user_id = ?" (Only userId)
    False -> query conn "select i.id, i.name, i.quantity, i.unit from nomz.ingredient i where i.user_id = ? and i.id in ?" (userId, In ingredientIds)
  pure $ foldr (\(ingredientId, name, quantity, unit) acc -> insertMap ingredientId (Ingredient name quantity unit) acc) mempty ingredients

insertIngredients :: Connection -> UserId -> [Ingredient] -> IO ()
insertIngredients conn userId ingredients =
  void $ executeMany conn "insert into nomz.ingredient (user_id, name, quantity, unit) values (?, ?, ?, ?)" $
    map (\Ingredient {..} -> (userId, ingredientName, ingredientQuantity, ingredientUnit)) ingredients

mergeIngredients :: Connection -> UserId -> [IngredientId] -> Ingredient -> IO ()
mergeIngredients conn userId ingredientIds ingredient = withTransaction conn $ do
  deleteIngredients conn userId ingredientIds
  insertIngredients conn userId [ingredient]

deleteIngredients :: Connection -> UserId -> [IngredientId] -> IO ()
deleteIngredients conn userId ingredientIds = do
  case null ingredientIds of
    True -> throwIO $ DatabaseException "No ingredient ids provided"
    False -> void $ execute conn "delete from nomz.ingredient where id in ? and user_id = ?" (In ingredientIds, userId)
