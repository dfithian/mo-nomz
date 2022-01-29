module Server where

import ClassyPrelude hiding (link)

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Logger (logError, runLoggingT)
import Network.URI (parseURI)
import Servant.API (NoContent(NoContent))
import Servant.Server (ServerError, err400, err401, err403, err404, err500, errReasonPhrase)
import qualified Data.Map as Map

import API.Types
  ( DeleteGroceryItemRequest(..), DeleteRecipeRequest(..), ExportGroceryItem(..)
  , ExportIngredient(..), ExportRecipe(..), ExportResponse(..), GetHealthResponse(..)
  , GroceryImportBlobRequest(..), ListGroceryItemResponse(..), ListRecipeResponse(..)
  , ListRecipeResponseV1(..), MergeGroceryItemRequest(..), ParseBlobRequest(..)
  , ParseBlobResponse(..), ParseLinkRequest(..), ParseLinkResponse(..), RecipeImportLinkRequest(..)
  , UpdateGroceryItemRequest(..), UpdateRecipeIngredientsRequest(..), UpdateRecipeRequest(..)
  , UserCreateResponse(..), ReadableRecipe
  )
import Auth (Authorization, generateToken, validateToken)
import Conversion
  ( mkOrderedIngredient, mkQuantity, mkReadableGroceryItem, mkReadableIngredient, mkReadableQuantity
  , mkReadableRecipe, mkReadableRecipeV1, mkReadableUnit, mkUnit
  )
import Foundation (AppM, appLogFunc, cacheSettings, settings, withDbConn)
import Parser (parseRawIngredients)
import Scrape (scrapeUrl)
import Scraper.Types (ScrapedRecipe(..))
import Settings (AppSettings(..), CacheSettings(..))
import Types
  ( GroceryItem(..), Ingredient(..), OrderedGroceryItem(..), OrderedIngredient(..), Recipe(..)
  , RecipeLink(..), RecipeId, UserId, ingredientToGroceryItem
  )
import qualified Database

scrapeUrlCached :: AppM m => RecipeLink -> m ScrapedRecipe
scrapeUrlCached link = do
  app <- ask
  let CacheSettings {..} = cacheSettings app
      scrape = do
        uri <- maybe (throwIO err400 { errReasonPhrase = "Invalid link" }) pure $ parseURI (unpack $ unRecipeLink link)
        recipeWithInfo@(ScrapedRecipe {..}, _) <- either (\e -> throwIO err500 { errReasonPhrase = unpack e }) pure
          =<< runExceptT (runLoggingT (runReaderT (scrapeUrl uri) app) (appLogFunc app))
        when (null scrapedRecipeIngredients) $ throwIO err400 { errReasonPhrase = "Failed to parse ingredients" }
        pure recipeWithInfo
  case cacheSettingsEnabled of
    False -> fst <$> liftIO scrape
    True -> unwrapDb $ withDbConn $ \c -> do
      Database.refreshCachedRecipes c cacheSettingsValidSeconds cacheSettingsMaxSize
      Database.selectCachedRecipe c link >>= \case
        Just cached -> pure cached
        Nothing -> do
          (cached, scrapeInfo) <- scrape
          Database.repsertCachedRecipe c link cached scrapeInfo
          pure cached

unwrapDb :: AppM m => m (Either SomeException a) -> m a
unwrapDb ma = ma >>= \case
  Right x -> pure x
  Left se -> case fromException se of
    Just (x :: ServerError) -> do
      $logError $ tshow x
      throwError x
    Nothing -> throwError err500

getHealth :: AppM m => m GetHealthResponse
getHealth = do
  unwrapDb $ withDbConn Database.health
  pure GetHealthResponse
    { getHealthResponseStatus = "ok"
    }

getRecentUsers :: AppM m => m (Int64, Int64, Int64, Int64)
getRecentUsers =
  unwrapDb $ withDbConn Database.selectRecentUsers

postCreateUser :: AppM m => m UserCreateResponse
postCreateUser = do
  AppSettings {..} <- asks settings
  (token, bcryptedToken) <- liftIO $ generateToken appBcryptCost
  result <- withDbConn $ \c -> Database.insertToken c bcryptedToken
  userId <- either (const $ throwError err401) pure result
  pure $ UserCreateResponse userId token

validateUserToken :: AppM m => Authorization -> UserId -> m ()
validateUserToken token userId = do
  withDbConn (\c -> Database.fetchToken c userId) >>= \case
    Right (Just bcryptedToken) -> case validateToken token bcryptedToken of
      Right True -> pure ()
      Right False -> throwError err403
      Left err -> do
        $logError $ "User token validation failed for " <> tshow userId <> " due to " <> pack err
        throwError err403
    _ -> do
      $logError $ "No user token for " <> tshow userId
      throwError err403

getGroceryItems :: AppM m => Authorization -> UserId -> m ListGroceryItemResponse
getGroceryItems token userId = do
  validateUserToken token userId
  groceryItems <- unwrapDb $ withDbConn $ \c -> Database.selectGroceryItems c userId []
  pure ListGroceryItemResponse
    { listGroceryItemResponseItems = mkReadableGroceryItem <$> groceryItems
    }

postUpdateGroceryItem :: AppM m => Authorization -> UserId -> UpdateGroceryItemRequest -> m NoContent
postUpdateGroceryItem token userId UpdateGroceryItemRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    let newGroceryItem = OrderedGroceryItem
          { orderedGroceryItemItem = GroceryItem
              { groceryItemName = updateGroceryItemRequestName
              , groceryItemQuantity = mkQuantity updateGroceryItemRequestQuantity
              , groceryItemUnit = mkUnit updateGroceryItemRequestUnit
              , groceryItemActive = updateGroceryItemRequestActive
              }
          , orderedGroceryItemOrder = updateGroceryItemRequestOrder
          }
    Database.updateOrderedGroceryItem c userId updateGroceryItemRequestId newGroceryItem
  pure NoContent

postMergeGroceryItem :: AppM m => Authorization -> UserId -> MergeGroceryItemRequest -> m NoContent
postMergeGroceryItem token userId MergeGroceryItemRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    existingIds <- asSet . setFromList . keys <$> Database.selectGroceryItems c userId (setToList mergeGroceryItemRequestIds)
    unless (null $ difference mergeGroceryItemRequestIds existingIds) $ throwIO err400
    let newGroceryItem = OrderedGroceryItem
          { orderedGroceryItemItem = GroceryItem
            { groceryItemName = mergeGroceryItemRequestName
            , groceryItemQuantity = mkQuantity mergeGroceryItemRequestQuantity
            , groceryItemUnit = mkUnit mergeGroceryItemRequestUnit
            , groceryItemActive = mergeGroceryItemRequestActive
            }
          , orderedGroceryItemOrder = mergeGroceryItemRequestOrder
          }
    void $ Database.mergeGroceryItems c userId (setToList mergeGroceryItemRequestIds) newGroceryItem
  pure NoContent

deleteGroceryItem :: AppM m => Authorization -> UserId -> DeleteGroceryItemRequest -> m NoContent
deleteGroceryItem token userId DeleteGroceryItemRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    existingIds <- asSet . setFromList . keys <$> Database.selectGroceryItems c userId (setToList deleteGroceryItemRequestIds)
    unless (null $ difference deleteGroceryItemRequestIds existingIds) $ throwIO err400
    Database.deleteGroceryItems c userId (setToList deleteGroceryItemRequestIds)
  pure NoContent

postClearGroceryItems :: AppM m => Authorization -> UserId -> m NoContent
postClearGroceryItems token userId = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    Database.deactivateEverything c userId
  pure NoContent

postRecipeImportLink :: AppM m => Authorization -> UserId -> RecipeImportLinkRequest -> m NoContent
postRecipeImportLink token userId RecipeImportLinkRequest {..} = do
  validateUserToken token userId
  existingRecipes <- unwrapDb $ withDbConn $ \c -> Database.selectRecipesByLink c userId recipeImportLinkRequestLink
  case headMay (mapToList existingRecipes) of
    Just (recipeId, Recipe {..}) ->
      case (recipeImportLinkRequestActive, recipeActive) of
        (True, True) -> pure ()
        (False, True) -> unwrapDb $ withDbConn $ \c -> Database.deactivateRecipe c userId recipeId
        (True, False) -> unwrapDb $ withDbConn $ \c -> Database.activateRecipe c userId recipeId
        (False, False) -> pure ()
    Nothing -> do
      ScrapedRecipe {..} <- scrapeUrlCached recipeImportLinkRequestLink
      let recipe = Recipe
            { recipeName = scrapedRecipeName
            , recipeLink = Just recipeImportLinkRequestLink
            , recipeActive = recipeImportLinkRequestActive
            , recipeRating = 0
            , recipeNotes = ""
            }
      unwrapDb $ withDbConn $ \c -> do
        groceryItemIds <- Database.insertGroceryItems c userId (ingredientToGroceryItem recipeImportLinkRequestActive <$> scrapedRecipeIngredients)
        void $ Database.insertRecipe c userId recipe $ zip groceryItemIds scrapedRecipeIngredients
        Database.automergeGroceryItems c userId
  pure NoContent

postGroceryImportBlob :: AppM m => Authorization -> UserId -> GroceryImportBlobRequest -> m NoContent
postGroceryImportBlob token userId GroceryImportBlobRequest {..} = do
  validateUserToken token userId
  ingredients <- either (\e -> throwError err500 { errReasonPhrase = unpack e }) pure $ parseRawIngredients groceryImportBlobRequestContent
  unwrapDb $ withDbConn $ \c -> do
    groceryItemIds <- Database.insertGroceryItems c userId (ingredientToGroceryItem True <$> ingredients)
    case groceryImportBlobRequestName of
      Nothing -> void $ Database.insertGroceryItemIngredients c userId $ zip groceryItemIds ingredients
      Just name -> do
        let recipe = Recipe
              { recipeName = name
              , recipeLink = groceryImportBlobRequestLink
              , recipeActive = True
              , recipeRating = 0
              , recipeNotes = ""
              }
        void $ Database.insertRecipe c userId recipe $ zip groceryItemIds ingredients
    Database.automergeGroceryItems c userId
  pure NoContent

postUpdateRecipe :: AppM m => Authorization -> UserId -> UpdateRecipeRequest -> m NoContent
postUpdateRecipe token userId UpdateRecipeRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    Recipe {..} <- maybe (throwIO err404) pure . headMay =<< Database.selectRecipes c userId [updateRecipeRequestId]
    Database.updateRecipe c userId updateRecipeRequestId (fromMaybe 0 updateRecipeRequestRating) (fromMaybe "" updateRecipeRequestNotes)
    when (updateRecipeRequestActive /= recipeActive) $
      case updateRecipeRequestActive of
        True -> Database.activateRecipe c userId updateRecipeRequestId
        False -> Database.deactivateRecipe c userId updateRecipeRequestId
  pure NoContent

postUpdateRecipeIngredients :: AppM m => Authorization -> UserId -> UpdateRecipeIngredientsRequest -> m NoContent
postUpdateRecipeIngredients token userId UpdateRecipeIngredientsRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    existing <- Database.selectRecipes c userId [updateRecipeIngredientsRequestId]
    let existingIds = asSet . setFromList . keys $ existing
        deleteIngredientIds = setToList updateRecipeIngredientsRequestDeletes
    Recipe {..} <- maybe (throwIO err400) pure $ lookup updateRecipeIngredientsRequestId existing
    unless (null $ difference (singletonSet updateRecipeIngredientsRequestId) existingIds) $ throwIO err400
    Database.unmergeGroceryItems c userId deleteIngredientIds
    Database.deleteIngredients c userId deleteIngredientIds
    let adds = mkOrderedIngredient <$> updateRecipeIngredientsRequestAdds
    case recipeActive of
      True -> do
        groceryItemIds <- Database.insertGroceryItems c userId (ingredientToGroceryItem recipeActive . orderedIngredientIngredient <$> adds)
        Database.insertIngredients c userId updateRecipeIngredientsRequestId $ zip groceryItemIds adds
      False -> Database.insertIngredientsNoGrocery c userId updateRecipeIngredientsRequestId adds
    Database.automergeGroceryItems c userId
  pure NoContent

getRecipesV1 :: AppM m => Authorization -> UserId -> m ListRecipeResponseV1
getRecipesV1 token userId = do
  validateUserToken token userId
  (recipes, ingredients) <- unwrapDb $ withDbConn $ \c -> (,)
    <$> Database.selectRecipes c userId []
    <*> Database.selectIngredientsByRecipeIds c userId []
  pure ListRecipeResponseV1
    { listRecipeResponseV1Recipes = mapFromList
        . map (\(recipeId, recipe) -> (recipeId, mkReadableRecipeV1 (orderedIngredientIngredient <$> Map.elems (findWithDefault mempty recipeId ingredients)) recipe))
        . mapToList
        $ recipes
    }

getRecipes :: AppM m => Authorization -> UserId -> m ListRecipeResponse
getRecipes token userId = do
  validateUserToken token userId
  (recipes, ingredients) <- unwrapDb $ withDbConn $ \c -> (,)
    <$> Database.selectRecipes c userId []
    <*> Database.selectIngredientsByRecipeIds c userId []
  pure ListRecipeResponse
    { listRecipeResponseRecipes = mapFromList
        . map (\(recipeId, recipe) -> (recipeId, mkReadableRecipe (findWithDefault mempty recipeId ingredients) recipe))
        . mapToList
        $ recipes
    }

getRecipe :: AppM m => Authorization -> UserId -> RecipeId -> m ReadableRecipe
getRecipe token userId recipeId = do
  validateUserToken token userId
  (recipes, ingredients) <- unwrapDb $ withDbConn $ \c -> (,)
    <$> Database.selectRecipes c userId [recipeId]
    <*> Database.selectIngredientsByRecipeId c userId recipeId
  case headMay recipes of
    Nothing -> throwError err404
    Just recipe -> pure $ mkReadableRecipe ingredients recipe

deleteRecipes :: AppM m => Authorization -> UserId -> DeleteRecipeRequest -> m NoContent
deleteRecipes token userId DeleteRecipeRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    existingIds <- asSet . setFromList . keys <$> Database.selectRecipes c userId (setToList deleteRecipeRequestIds)
    unless (null $ difference deleteRecipeRequestIds existingIds) $ throwIO err400
    ingredientIds <- Database.selectRecipeIngredientIds c userId (setToList deleteRecipeRequestIds)
    Database.unmergeGroceryItems c userId ingredientIds
    Database.deleteRecipes c userId (setToList deleteRecipeRequestIds)
  pure NoContent

-- parsing only
postParseBlob :: AppM m => Authorization -> UserId -> ParseBlobRequest -> m ParseBlobResponse
postParseBlob token userId ParseBlobRequest {..} = do
  validateUserToken token userId
  ingredients <- either (\e -> throwError err500 { errReasonPhrase = unpack e }) pure $ parseRawIngredients parseBlobRequestContent
  pure ParseBlobResponse
    { parseBlobResponseIngredients = mkReadableIngredient <$> zipWith OrderedIngredient ingredients [1..]
    }

postParseLink :: AppM m => Authorization -> UserId -> ParseLinkRequest -> m ParseLinkResponse
postParseLink token userId ParseLinkRequest {..} = do
  validateUserToken token userId
  ScrapedRecipe {..} <- scrapeUrlCached parseLinkRequestLink
  pure ParseLinkResponse
    { parseLinkResponseName = scrapedRecipeName
    , parseLinkResponseIngredients = mkReadableIngredient <$> zipWith OrderedIngredient scrapedRecipeIngredients [1..]
    , parseLinkResponseSteps = scrapedRecipeSteps
    }

-- export data
getExport :: AppM m => Authorization -> UserId -> m ExportResponse
getExport token userId = do
  validateUserToken token userId
  (groceries, recipes, ingredients) <- unwrapDb $ withDbConn $ \c -> do
    groceries <- Database.selectGroceryItems c userId []
    recipes <- Database.selectRecipes c userId []
    ingredients <- Database.selectIngredients c userId []
    Database.exportConfirm c userId
    pure (groceries, recipes, ingredients)
  pure ExportResponse
    { exportResponseGroceries = map (\OrderedGroceryItem {..} ->
        let GroceryItem {..} = orderedGroceryItemItem
        in ExportGroceryItem
          { exportGroceryItemName = groceryItemName
          , exportGroceryItemQuantity = mkReadableQuantity groceryItemQuantity
          , exportGroceryItemUnit = mkReadableUnit groceryItemUnit
          , exportGroceryItemActive = groceryItemActive
          , exportGroceryItemOrder = orderedGroceryItemOrder
          }
      ) groceries
    , exportResponseRecipes = map (\Recipe {..} ->
        ExportRecipe
          { exportRecipeName = recipeName
          , exportRecipeLink = recipeLink
          , exportRecipeActive = recipeActive
          , exportRecipeRating = recipeRating
          , exportRecipeNotes = recipeNotes
          }
      ) recipes
    , exportResponseIngredients = map (\(recipeId, groceryItemId, OrderedIngredient {..}) ->
        let Ingredient {..} = orderedIngredientIngredient
        in ExportIngredient
          { exportIngredientGroceryItemId = groceryItemId
          , exportIngredientRecipeId = recipeId
          , exportIngredientName = ingredientName
          , exportIngredientQuantity = mkReadableQuantity ingredientQuantity
          , exportIngredientUnit = mkReadableUnit ingredientUnit
          , exportIngredientOrder = orderedIngredientOrder
          }
      ) ingredients
    }
