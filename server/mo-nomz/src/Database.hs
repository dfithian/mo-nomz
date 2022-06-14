module Database where

import NomzPrelude

import Chez.Grater.Scraper.Types (ScrapeMeta(..), ScrapeMetaWrapper(..), ScrapeName, ScrapeVersion)
import Chez.Grater.Types (Ingredient(..))
import Data.Serialize (decode, encode)
import Database.PostgreSQL.Simple
  ( Binary(Binary), In(In), Only(Only), Connection, execute, fromOnly, query, query_, returning
  )
import qualified Data.Map.Strict as Map

import Auth (BcryptedAuthorization)
import Postgres.Orphans ()
import Types
  ( GroceryItem(..), OrderedGroceryItem(..), OrderedIngredient(..), Recipe(..), ScrapedRecipe(..)
  , GroceryItemId, IngredientId, RecipeId, RecipeLink, UserId
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

updateUserPing :: Connection -> UserId -> Text -> Maybe Text -> IO ()
updateUserPing conn userId version targetMay = do
  void $ execute conn "update nomz.user set version = ?, target = ? where id = ?" (version, targetMay, userId)

selectRecentUsers :: Connection -> IO (Int, Int, Int, Int)
selectRecentUsers conn = do
  let q interval = maybe 0 fromOnly . headMay <$> query_ conn ("select count(id) from nomz.user where last_active >= now () - interval '" <> interval <> "' and coalesce(target, 'device') <> 'simulator'")
  (,,,)
    <$> q "1 day"
    <*> q "7 day"
    <*> q "28 day"
    <*> q "365 day"

selectCacheStats :: Connection -> IO (Int, Maybe UTCTime, Maybe UTCTime)
selectCacheStats conn = do
  fromMaybe (0, Nothing, Nothing) . headMay
    <$> query_ conn "select count(*), max(updated), min(updated) from nomz.recipe_cache"

selectGroceryItems :: Connection -> UserId -> [GroceryItemId] -> IO (Map GroceryItemId OrderedGroceryItem)
selectGroceryItems conn userId groceryItemIds = do
  groceryItems <- case null groceryItemIds of
    True -> query conn "select id, name, quantity, unit, active, ordering from nomz.grocery_item where user_id = ? order by ordering, name" (Only userId)
    False -> query conn "select id, name, quantity, unit, active, ordering from nomz.grocery_item where user_id = ? and id in ? order by ordering, name" (userId, In groceryItemIds)
  pure $ foldr (\(groceryItemId, name, quantity, unit, active, order) acc -> Map.insert groceryItemId (OrderedGroceryItem (GroceryItem name quantity unit active) order) acc) mempty groceryItems

selectRecipes :: Connection -> UserId -> [RecipeId] -> IO (Map RecipeId Recipe)
selectRecipes conn userId recipeIds = do
  case null recipeIds of
    True -> do
      recipes <- query conn "select id, name, link, active, rating, notes from nomz.recipe where user_id = ? order by id" (Only userId)
      pure . Map.fromList . fmap (\(recipeId, name, link, active, rating, notes) -> (recipeId, Recipe name link active rating notes)) $ recipes
    False -> do
      recipes <- query conn "select id, name, link, active, rating, notes from nomz.recipe where user_id = ? and id in ? order by id" (userId, In recipeIds)
      pure . Map.fromList . fmap (\(recipeId, name, link, active, rating, notes) -> (recipeId, Recipe name link active rating notes)) $ recipes

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

repsertCachedRecipe :: Connection -> RecipeLink -> ScrapedRecipe -> ScrapeMetaWrapper -> IO ()
repsertCachedRecipe conn link recipe info = do
  now <- getCurrentTime
  void $ execute conn "delete from nomz.recipe_cache where link = ?" (Only link)
  case info of
    ScrapeMetaWrapperIngredient ingredient ->
      void $ execute conn
        "insert into nomz.recipe_cache (link, data, updated, ingredient_scrape_name, ingredient_scrape_version) values (?, ?, ?, ?, ?)"
        (link, Binary (encode recipe), now, scrapeMetaName ingredient, scrapeMetaVersion ingredient)
    ScrapeMetaWrapperIngredientAndStep ingredient step ->
      void $ execute conn
        "insert into nomz.recipe_cache (link, data, updated, ingredient_scrape_name, ingredient_scrape_version, step_scrape_name, step_scrape_version) values (?, ?, ?, ?, ?, ?, ?)"
        (link, Binary (encode recipe), now, scrapeMetaName ingredient, scrapeMetaVersion ingredient, scrapeMetaName step, scrapeMetaVersion step)

refreshCachedRecipes :: Connection -> Int -> Int -> IO ()
refreshCachedRecipes conn validityWindow maxSize = do
  validTime <- addUTCTime (negate (fromIntegral validityWindow)) <$> getCurrentTime
  void $ execute conn "delete from nomz.recipe_cache where updated < ?" (Only validTime)
  void $ execute conn "delete from nomz.recipe_cache where link in (select link from nomz.recipe_cache order by updated desc offset ?)" (Only maxSize)

invalidateCachedRecipes :: Connection -> (ScrapeMetaWrapper -> Bool) -> IO ()
invalidateCachedRecipes conn isInvalid = do
  let getInvalid :: (RecipeLink, Maybe ScrapeName, Maybe ScrapeVersion, Maybe ScrapeName, Maybe ScrapeVersion) -> Maybe RecipeLink
      getInvalid = \case
        (link, Just ingredientName, Just ingredientVersion, Nothing, Nothing) -> if isInvalid (ScrapeMetaWrapperIngredient (ScrapeMeta ingredientName ingredientVersion)) then Just link else Nothing
        (link, Just ingredientName, Just ingredientVersion, Just stepName, Just stepVersion) -> if isInvalid (ScrapeMetaWrapperIngredientAndStep (ScrapeMeta ingredientName ingredientVersion) (ScrapeMeta stepName stepVersion)) then Just link else Nothing
        (link, _, _, _, _) -> Just link
  links <- mapMaybe getInvalid <$> query_ conn "select link, ingredient_scrape_name, ingredient_scrape_version, step_scrape_name, step_scrape_version from nomz.recipe_cache"
  void $ execute conn "delete from nomz.recipe_cache where link in ?" (Only (In links))
