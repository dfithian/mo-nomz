module Database where

import Prelude

import Control.Exception (Exception)
import Control.Monad (unless, void)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Serialize (decode, encode)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
  ( Binary(Binary), In(In), Only(Only), Connection, execute, executeMany, fromOnly, query, query_
  , returning
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Auth (BcryptedAuthorization)
import Conversion (combineItems)
import Scraper.Types (ScrapeInfo(..), ScrapedInfo(..), ScrapeName, ScrapeVersion, ScrapedRecipe)
import Types
  ( GroceryItem(..), Ingredient(..), OrderedGroceryItem(..), OrderedIngredient(..), Recipe(..)
  , GroceryItemId, IngredientId, RecipeId, RecipeLink, UserId, headMay, ingredientToGroceryItem
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
  token <- query conn "select token from nomz.user where id = ? and is_valid" (Only userId) >>= \case
    [(Only token)] -> pure token
    _ -> pure Nothing
  void $ execute conn "update nomz.user set last_active = now() where id = ?" (Only userId)
  pure token

selectRecentUsers :: Connection -> IO (Int, Int, Int, Int)
selectRecentUsers conn = do
  let q interval = maybe 0 fromOnly . headMay <$> query_ conn ("select count(id) from nomz.user where last_active >= now () - interval '" <> interval <> "'")
  (,,,)
    <$> q "1 day"
    <*> q "7 day"
    <*> q "28 day"
    <*> q "365 day"

selectGroceryItems :: Connection -> UserId -> [GroceryItemId] -> IO (Map GroceryItemId OrderedGroceryItem)
selectGroceryItems conn userId groceryItemIds = do
  groceryItems <- case null groceryItemIds of
    True -> query conn "select id, name, quantity, unit, active, ordering from nomz.grocery_item where user_id = ? order by ordering, name" (Only userId)
    False -> query conn "select id, name, quantity, unit, active, ordering from nomz.grocery_item where user_id = ? and id in ? order by ordering, name" (userId, In groceryItemIds)
  pure $ foldr (\(groceryItemId, name, quantity, unit, active, order) acc -> Map.insert groceryItemId (OrderedGroceryItem (GroceryItem name quantity unit active) order) acc) mempty groceryItems

selectMaxOrder :: Connection -> UserId -> IO Int
selectMaxOrder conn userId =
  query conn "select max(ordering) from nomz.grocery_item where user_id = ?" (Only userId) >>= \case
    (Just (Only x)):_ -> pure x
    _ -> pure 0

insertGroceryItems :: Connection -> UserId -> [GroceryItem] -> IO [GroceryItemId]
insertGroceryItems conn userId groceryItems = do
  maxOrder <- selectMaxOrder conn userId
  insertOrderedGroceryItems conn userId $ zipWith OrderedGroceryItem groceryItems [(maxOrder + 1)..]

insertOrderedGroceryItems :: Connection -> UserId -> [OrderedGroceryItem] -> IO [GroceryItemId]
insertOrderedGroceryItems conn userId groceryItems = do
  fmap (fmap fromOnly)
    . returning conn "insert into nomz.grocery_item (user_id, name, quantity, unit, active, ordering) values (?, ?, ?, ?, ?, ?) returning id"
    . fmap ( \(OrderedGroceryItem {..}) ->
        let GroceryItem {..} = orderedGroceryItemItem
        in (userId, groceryItemName, groceryItemQuantity, groceryItemUnit, groceryItemActive, orderedGroceryItemOrder)
      )
    $ groceryItems

updateOrderedGroceryItem :: Connection -> UserId -> GroceryItemId -> OrderedGroceryItem -> IO ()
updateOrderedGroceryItem conn userId groceryItemId OrderedGroceryItem {..} = do
  let GroceryItem {..} = orderedGroceryItemItem
  void $ execute conn
    "update nomz.grocery_item set name = ?, quantity = ?, unit = ?, active = ?, ordering = ? where user_id = ? and id = ?"
    (groceryItemName, groceryItemQuantity, groceryItemUnit, groceryItemActive, orderedGroceryItemOrder, userId, groceryItemId)
  void $ execute conn
    "update nomz.grocery_item set ordering = 1 + ordering where user_id = ? and ordering >= ? and id <> ?"
    (userId, orderedGroceryItemOrder, groceryItemId)

mergeGroceryItems :: Connection -> UserId -> [GroceryItemId] -> OrderedGroceryItem -> IO GroceryItemId
mergeGroceryItems conn userId oldGroceryItemIds newGroceryItem = do
  [newGroceryItemId] <- insertOrderedGroceryItems conn userId [newGroceryItem]
  mergeIngredientGroceryItemIds conn userId oldGroceryItemIds newGroceryItemId
  deleteGroceryItems conn userId oldGroceryItemIds
  pure newGroceryItemId

automergeGroceryItems :: Connection -> UserId -> IO ()
automergeGroceryItems c userId = do
  (activeGroceryItems, inactiveGroceryItems) <- Map.partition (groceryItemActive . orderedGroceryItemItem) <$> selectGroceryItems c userId []
  let remap (groceryItemId, OrderedGroceryItem {..}) =
        let GroceryItem {..} = orderedGroceryItemItem
        in Map.insertWith (<>) (groceryItemName, groceryItemUnit) (Set.singleton groceryItemId)
      activeGroceryItemIdsByNameAndUnit = foldr remap mempty . Map.toList $ activeGroceryItems
      inactiveGroceryItemIdsByNameAndUnit = foldr remap mempty . Map.toList $ inactiveGroceryItems
      combinedActiveGroceryItems = combineItems $ Map.elems activeGroceryItems
      combinedInactiveGroceryItems = combineItems $ Map.elems inactiveGroceryItems
      (newActiveGroceryItems, oldActiveGroceryItemIds) = unzip $ flip mapMaybe combinedActiveGroceryItems $ \x@OrderedGroceryItem {..} ->
        let GroceryItem {..} = orderedGroceryItemItem
        in case Map.lookup (groceryItemName, groceryItemUnit) activeGroceryItemIdsByNameAndUnit of
          Just xs | length xs >= 2 -> Just (x, Set.toList xs)
          _ -> Nothing
      (newInactiveGroceryItems, oldInactiveGroceryItemIds) = unzip $ flip mapMaybe combinedInactiveGroceryItems $ \x@OrderedGroceryItem {..} ->
        let GroceryItem {..} = orderedGroceryItemItem
        in case Map.lookup (groceryItemName, groceryItemUnit) inactiveGroceryItemIdsByNameAndUnit of
          Just xs | length xs >= 2 -> Just (x, Set.toList xs)
          _ -> Nothing
  newGroceryItemIds <- insertOrderedGroceryItems c userId (newActiveGroceryItems <> newInactiveGroceryItems)
  for_ (zip newGroceryItemIds (oldActiveGroceryItemIds <> oldInactiveGroceryItemIds)) $ \(newGroceryItemId, oldGroceryItemIds) ->
    mergeIngredientGroceryItemIds c userId oldGroceryItemIds newGroceryItemId
  deleteGroceryItems c userId (mconcat oldActiveGroceryItemIds <> mconcat oldInactiveGroceryItemIds)

unmergeGroceryItems :: Connection -> UserId -> [IngredientId] -> IO ()
unmergeGroceryItems conn userId oldIngredientIds =
  case null oldIngredientIds of
    True -> pure mempty
    False -> do
      oldGroceryItemIds <- fmap fromOnly
        <$> query conn "select distinct(grocery_id) from nomz.ingredient where user_id = ? and id in ? and grocery_id is not null" (userId, In oldIngredientIds)
      void $ execute conn "update nomz.ingredient set grocery_id = null where user_id = ? and id in ?" (userId, In oldIngredientIds)
      (unmergedIngredientIds, newGroceryItems) <- unzip . fmap (\(ingredientId, name, quantity, unit, active, order) -> (ingredientId, OrderedGroceryItem (GroceryItem name quantity unit active) order))
        <$> query conn "select i.id, g.name, i.quantity, i.unit, g.active, g.ordering from nomz.ingredient i join nomz.grocery_item g on g.id = i.grocery_id where i.user_id = ? and i.grocery_id in ?" (userId, In oldGroceryItemIds)

      newGroceryItemIds <- insertOrderedGroceryItems conn userId newGroceryItems
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
  fmap (fmap fromOnly)
    . returning conn "insert into nomz.ingredient (grocery_id, user_id, name, quantity, unit) values (?, ?, ?, ?, ?) returning id"
    . fmap (\(groceryItemId, Ingredient {..}) -> (groceryItemId, userId, ingredientName, ingredientQuantity, ingredientUnit))
    $ ingredients

selectIngredientsByRecipeId :: Connection -> UserId -> RecipeId -> IO (Map IngredientId OrderedIngredient)
selectIngredientsByRecipeId conn userId recipeId = do
  ingredients <- query conn "select id, name, quantity, unit, ordering from nomz.ingredient where user_id = ? and recipe_id = ? order by ordering, name" (userId, recipeId)
  pure
    . Map.fromList
    . fmap (\(defaultOrder, (ingredientId, name, quantity, unit, order)) -> (ingredientId, OrderedIngredient (Ingredient name quantity unit) (fromMaybe defaultOrder order)))
    . zip [1..]
    $ ingredients

selectRecipeIngredientIds :: Connection -> UserId -> [RecipeId] -> IO [IngredientId]
selectRecipeIngredientIds conn userId recipeIds =
  case null recipeIds of
    True -> pure []
    False -> fmap fromOnly
      <$> query conn "select id from nomz.ingredient where user_id = ? and recipe_id in ?" (userId, In recipeIds)

selectIngredientsByRecipeIds :: Connection -> UserId -> [RecipeId] -> IO (Map RecipeId (Map IngredientId OrderedIngredient))
selectIngredientsByRecipeIds conn userId recipeIds = do
  ingredients <- case null recipeIds of
    True -> query conn "select recipe_id, id, name, quantity, unit, ordering from nomz.ingredient where user_id = ? and recipe_id is not null order by ordering, name" (Only userId)
    False -> query conn "select recipe_id, id, name, quantity, unit, ordering from nomz.ingredient where user_id = ? and recipe_id is not null and recipe_id in ? order by ordering, name" (userId, In recipeIds)
  pure
    . fmap Map.fromList
    . Map.fromListWith (<>)
    . fmap (\(defaultOrder, (recipeId, ingredientId, name, quantity, unit, order)) -> (recipeId, [(ingredientId, OrderedIngredient (Ingredient name quantity unit) (fromMaybe defaultOrder order))]))
    . zip [1..]
    $ ingredients

selectRecipes :: Connection -> UserId -> [RecipeId] -> IO (Map RecipeId Recipe)
selectRecipes conn userId recipeIds = do
  case null recipeIds of
    True -> do
      recipes <- query conn "select id, name, link, active, rating, notes from nomz.recipe where user_id = ? order by id" (Only userId)
      pure . Map.fromList . fmap (\(recipeId, name, link, active, rating, notes) -> (recipeId, Recipe name link active rating notes)) $ recipes
    False -> do
      recipes <- query conn "select id, name, link, active, rating, notes from nomz.recipe where user_id = ? and id in ? order by id" (userId, In recipeIds)
      pure . Map.fromList . fmap (\(recipeId, name, link, active, rating, notes) -> (recipeId, Recipe name link active rating notes)) $ recipes

selectRecipesByLink :: Connection -> UserId -> RecipeLink -> IO (Map RecipeId Recipe)
selectRecipesByLink conn userId recipeLink = do
  recipes <- query conn "select id, name, link, active, rating, notes from nomz.recipe where user_id = ? and link = ?" (userId, recipeLink)
  pure . Map.fromList . fmap (\(recipeId, name, link, active, rating, notes) -> (recipeId, Recipe name link active rating notes)) $ recipes

insertIngredients :: Connection -> UserId -> RecipeId -> [(GroceryItemId, OrderedIngredient)] -> IO ()
insertIngredients conn userId recipeId ingredients = do
  void $ executeMany conn "insert into nomz.ingredient (recipe_id, grocery_id, user_id, name, quantity, unit, ordering) values (?, ?, ?, ?, ?, ?, ?)" $
    fmap (\(groceryItemId, OrderedIngredient {..}) -> let Ingredient {..} = orderedIngredientIngredient in (recipeId, groceryItemId, userId, ingredientName, ingredientQuantity, ingredientUnit, orderedIngredientOrder)) ingredients

insertIngredientsNoGrocery :: Connection -> UserId -> RecipeId -> [OrderedIngredient] -> IO ()
insertIngredientsNoGrocery conn userId recipeId ingredients = do
  void $ executeMany conn "insert into nomz.ingredient (recipe_id, user_id, name, quantity, unit, ordering) values (?, ?, ?, ?, ?, ?)" $
    fmap (\OrderedIngredient {..} -> let Ingredient {..} = orderedIngredientIngredient in (recipeId, userId, ingredientName, ingredientQuantity, ingredientUnit, orderedIngredientOrder)) ingredients

insertRecipe :: Connection -> UserId -> Recipe -> [(GroceryItemId, Ingredient)] -> IO RecipeId
insertRecipe conn userId Recipe {..} ingredients = do
  [(Only (recipeId :: RecipeId))] <- returning conn "insert into nomz.recipe (user_id, name, link, active, rating, notes) values (?, ?, ?, ?, ?, ?) returning id" [(userId, recipeName, recipeLink, recipeActive, recipeRating, recipeNotes)]
  insertIngredients conn userId recipeId (zipWith (\(groceryItemId, ingredient) order -> (groceryItemId, OrderedIngredient ingredient order)) ingredients [1..])
  pure recipeId

deactivateRecipe :: Connection -> UserId -> RecipeId -> IO ()
deactivateRecipe conn userId recipeId = do
  ingredientIds <- selectRecipeIngredientIds conn userId [recipeId]
  unmergeGroceryItems conn userId ingredientIds
  unless (null ingredientIds) $ do
    void $ execute conn "update nomz.ingredient set grocery_id = null where user_id = ? and id in ?" (userId, In ingredientIds)
  void $ execute conn "update nomz.recipe set active = false where user_id = ? and id = ?" (userId, recipeId)

activateRecipe :: Connection -> UserId -> RecipeId -> IO ()
activateRecipe conn userId recipeId = do
  (ingredientIds, ingredients) <- unzip . Map.toList <$> selectIngredientsByRecipeId conn userId recipeId
  groceryItemIds <- insertGroceryItems conn userId (ingredientToGroceryItem True . orderedIngredientIngredient <$> ingredients)
  for_ (zip groceryItemIds ingredientIds) $ \(groceryItemId, ingredientId) ->
    void $ execute conn "update nomz.ingredient set grocery_id = ? where user_id = ? and id = ?" (groceryItemId, userId, ingredientId)
  void $ execute conn "update nomz.recipe set active = true where user_id = ? and id = ?" (userId, recipeId)

updateRecipe :: Connection -> UserId -> RecipeId -> Int -> Text -> IO ()
updateRecipe conn userId recipeId rating notes =
  void $ execute conn "update nomz.recipe set rating = ?, notes = ? where user_id = ? and id = ?" (rating, notes, userId, recipeId)

deleteIngredients :: Connection -> UserId -> [IngredientId] -> IO ()
deleteIngredients conn userId ingredientIds = do
  case null ingredientIds of
    True -> pure ()
    False -> void $ execute conn "delete from nomz.ingredient where user_id = ? and id in ?" (userId, In ingredientIds)

deleteRecipes :: Connection -> UserId -> [RecipeId] -> IO ()
deleteRecipes conn userId recipeIds = do
  case null recipeIds of
    True -> pure ()
    False -> void $ execute conn "delete from nomz.recipe where user_id = ? and id in ?" (userId, In recipeIds)

selectIngredients :: Connection -> UserId -> [IngredientId] -> IO (Map IngredientId (Maybe RecipeId, Maybe GroceryItemId, OrderedIngredient))
selectIngredients conn userId ingredientIds = do
  ingredients <- case null ingredientIds of
    True -> query conn "select id, recipe_id, grocery_id, name, quantity, unit, ordering from nomz.ingredient where user_id = ? order by recipe_id, ordering, name" (Only userId)
    False -> query conn "select id, recipe_id, grocery_id, name, quantity, unit, ordering from nomz.ingredient where user_id = ? and id in ?" (userId, In ingredientIds)
  pure
    . Map.fromList
    . fmap (\(defaultOrder, (ingredientId, recipeId, groceryItemId, name, quantity, unit, order)) -> (ingredientId, (recipeId, groceryItemId, OrderedIngredient (Ingredient name quantity unit) (fromMaybe defaultOrder order))))
    . zip [1..]
    $ ingredients

exportConfirm :: Connection -> UserId -> IO ()
exportConfirm conn userId = do
  now <- getCurrentTime
  void $ execute conn "insert into nomz.export (user_id, confirmed_at) values (?, ?)" (userId, now)

selectCachedRecipe :: Connection -> RecipeLink -> IO (Maybe ScrapedRecipe)
selectCachedRecipe conn link = do
  query conn "select data from nomz.recipe_cache where link = ?" (Only link) >>= \case
    [Only (Binary data_)] -> pure . either (const Nothing) Just . decode $ data_
    _ -> pure Nothing

repsertCachedRecipe :: Connection -> RecipeLink -> ScrapedRecipe -> ScrapedInfo -> IO ()
repsertCachedRecipe conn link recipe info = do
  now <- getCurrentTime
  void $ execute conn "delete from nomz.recipe_cache where link = ?" (Only link)
  case info of
    ScrapedInfoIngredient ingredient ->
      void $ execute conn
        "insert into nomz.recipe_cache (link, data, updated, ingredient_scrape_name, ingredient_scrape_version) values (?, ?, ?, ?, ?)"
        (link, Binary (encode recipe), now, scrapeInfoName ingredient, scrapeInfoVersion ingredient)
    ScrapedInfoIngredientStep ingredient step ->
      void $ execute conn
        "insert into nomz.recipe_cache (link, data, updated, ingredient_scrape_name, ingredient_scrape_version, step_scrape_name, step_scrape_version) values (?, ?, ?, ?, ?, ?, ?)"
        (link, Binary (encode recipe), now, scrapeInfoName ingredient, scrapeInfoVersion ingredient, scrapeInfoName step, scrapeInfoVersion step)

refreshCachedRecipes :: Connection -> Int -> Int -> IO ()
refreshCachedRecipes conn validityWindow maxSize = do
  validTime <- addUTCTime (negate (fromIntegral validityWindow)) <$> getCurrentTime
  void $ execute conn "delete from nomz.recipe_cache where updated < ?" (Only validTime)
  void $ execute conn "delete from nomz.recipe_cache where link in (select link from nomz.recipe_cache order by updated desc offset ?)" (Only maxSize)

invalidateCachedRecipes :: Connection -> (ScrapedInfo -> Bool) -> IO ()
invalidateCachedRecipes conn isInvalid = do
  let getInvalid :: (RecipeLink, Maybe ScrapeName, Maybe ScrapeVersion, Maybe ScrapeName, Maybe ScrapeVersion) -> Maybe RecipeLink
      getInvalid = \case
        (link, Just ingredientName, Just ingredientVersion, Nothing, Nothing) -> if isInvalid (ScrapedInfoIngredient (ScrapeInfo ingredientName ingredientVersion)) then Just link else Nothing
        (link, Just ingredientName, Just ingredientVersion, Just stepName, Just stepVersion) -> if isInvalid (ScrapedInfoIngredientStep (ScrapeInfo ingredientName ingredientVersion) (ScrapeInfo stepName stepVersion)) then Just link else Nothing
        (link, _, _, _, _) -> Just link
  links <- mapMaybe getInvalid <$> query_ conn "select link, ingredient_scrape_name, ingredient_scrape_version, step_scrape_name, step_scrape_version from nomz.recipe_cache"
  void $ execute conn "delete from nomz.recipe_cache where link in ?" (Only (In links))
