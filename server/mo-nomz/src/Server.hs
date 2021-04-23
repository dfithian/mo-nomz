module Server where

import ClassyPrelude

import Control.Monad.Except (throwError)
import Control.Monad.Logger (logError)
import Database.PostgreSQL.Simple (Connection)
import Network.URI (parseURI)
import Servant.API (NoContent(NoContent))
import Servant.Server (ServerError, err400, err401, err403, err404, err500, errReasonPhrase)
import qualified Data.Map as Map

import API.Types
  ( DeleteGroceryItemRequest(..), DeleteRecipeRequest(..), GetHealthResponse(..)
  , GroceryImportBlobRequest(..), GroceryImportListRequest(..), GroceryImportSingle(..)
  , ListGroceryItemResponse(..), ListRecipeResponse(..), MergeGroceryItemRequest(..)
  , ReadableGroceryItem(..), ReadableGroceryItemAggregate(..), RecipeImportLinkRequest(..)
  , UpdateRecipeRequest(..), UserCreateResponse(..)
  )
import Auth (Authorization, generateToken, validateToken)
import Conversion (combineIngredients, mkQuantity, mkReadableGroceryItem, mkReadableRecipe, mkUnit)
import Foundation (AppM, settings, withDbConn)
import Scrape (ScrapedRecipe(..), parseIngredients, scrapeUrl)
import Settings (AppSettings(..))
import Types
  ( GroceryItem(..), Ingredient(..), Recipe(..), RecipeLink(..), UserId, ingredientToGroceryItem
  , mapError
  )
import qualified Database

unwrapDb :: AppM m => m (Either SomeException a) -> m a
unwrapDb ma = ma >>= \case
  Right x -> pure x
  Left se -> case fromException se of
    Just (x :: ServerError) -> throwError x
    Nothing -> throwError err500

getHealth :: AppM m => m GetHealthResponse
getHealth = do
  unwrapDb $ withDbConn Database.health
  pure GetHealthResponse
    { getHealthResponseStatus = "ok"
    }

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
        $logError $ "User token validation failed due to " <> pack err
        throwError err403
    _ -> throwError err403

getGroceryItems :: AppM m => Authorization -> UserId -> m ListGroceryItemResponse
getGroceryItems token userId = do
  validateUserToken token userId
  groceryItems <- unwrapDb $ withDbConn $ \c -> Database.selectGroceryItems c userId []
  let readableGroceryItems = sortOn (readableGroceryItemName . readableGroceryItemAggregateItem)
        . map (uncurry ReadableGroceryItemAggregate . first singleton)
        . mapToList . map mkReadableGroceryItem
        $ groceryItems
  pure ListGroceryItemResponse
    { listGroceryItemResponseItems = readableGroceryItems
    }

postMergeGroceryItem :: AppM m => Authorization -> UserId -> MergeGroceryItemRequest -> m NoContent
postMergeGroceryItem token userId MergeGroceryItemRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    existingIds <- asSet . setFromList . keys <$> Database.selectGroceryItems c userId (setToList mergeGroceryItemRequestIds)
    unless (null $ difference mergeGroceryItemRequestIds existingIds) $ throwIO err400
    let newGroceryItem = GroceryItem
          { groceryItemName = mergeGroceryItemRequestName
          , groceryItemQuantity = mkQuantity mergeGroceryItemRequestQuantity
          , groceryItemUnit = mkUnit mergeGroceryItemRequestUnit
          , groceryItemActive = mergeGroceryItemRequestActive
          }
    Database.mergeGroceryItems c userId (setToList mergeGroceryItemRequestIds) newGroceryItem
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
    Database.clearGroceryItems c userId
    Database.updateRecipes c userId [] False
  pure NoContent

postRecipeImportLink :: AppM m => Authorization -> UserId -> RecipeImportLinkRequest -> m NoContent
postRecipeImportLink token userId RecipeImportLinkRequest {..} = do
  validateUserToken token userId
  uri <- maybe (throwError err400 { errReasonPhrase = "Invalid link" }) pure $ parseURI (unpack $ unRecipeLink recipeImportLinkRequestLink)
  ScrapedRecipe {..} <- mapError (\e -> err500 { errReasonPhrase = unpack e }) $ scrapeUrl uri
  when (null scrapedRecipeIngredients) $ throwError err400 { errReasonPhrase = "Failed to parse ingredients" }
  let recipe = Recipe
        { recipeName = scrapedRecipeName
        , recipeLink = (Just recipeImportLinkRequestLink)
        , recipeActive = True
        }
  unwrapDb $ withDbConn $ \c -> do
    Database.insertRecipe c userId recipe scrapedRecipeIngredients
    refreshGroceryItems c userId
  pure NoContent

postGroceryImportList :: AppM m => Authorization -> UserId -> GroceryImportListRequest -> m NoContent
postGroceryImportList token userId GroceryImportListRequest {..} = do
  validateUserToken token userId
  let ingredients = flip map groceryImportListRequestItems $ \GroceryImportSingle {..} -> Ingredient
        { ingredientName = groceryImportSingleName
        , ingredientQuantity = mkQuantity groceryImportSingleQuantity
        , ingredientUnit = mkUnit groceryImportSingleUnit
        }
  unwrapDb $ withDbConn $ \c -> do
    groceryItemIds <- Database.insertGroceryItems c userId (ingredientToGroceryItem <$> ingredients)
    Database.insertGroceryItemIngredients c userId $ zip groceryItemIds ingredients
    refreshGroceryItems c userId
  pure NoContent

postGroceryImportBlob :: AppM m => Authorization -> UserId -> GroceryImportBlobRequest -> m NoContent
postGroceryImportBlob token userId GroceryImportBlobRequest {..} = do
  validateUserToken token userId
  ingredients <- mapError (\e -> err500 { errReasonPhrase = unpack e }) $ parseIngredients groceryImportBlobRequestContent
  unwrapDb $ withDbConn $ \c -> do
    groceryItemIds <- Database.insertGroceryItems c userId (ingredientToGroceryItem <$> ingredients)
    Database.insertGroceryItemIngredients c userId $ zip groceryItemIds ingredients
    refreshGroceryItems c userId
  pure NoContent

refreshGroceryItems :: Connection -> UserId -> IO ()
refreshGroceryItems c userId = do
  existingGroceryItems <- Database.selectGroceryItems c userId []
  ingredients <- Database.selectActiveIngredients c userId
  let remap (groceryItemId, GroceryItem {..}) = insertWith (<>) (groceryItemName, groceryItemUnit) (asSet $ singletonSet groceryItemId)
      existingGroceryItemIdsByNameAndUnit = asMap . foldr remap mempty . mapToList $ existingGroceryItems
      groceryItems = ingredientToGroceryItem <$> combineIngredients ingredients
  newGroceryItemIds <- Database.insertGroceryItems c userId groceryItems
  for_ (zip newGroceryItemIds groceryItems) $ \(groceryItemId, GroceryItem {..}) -> do
    let existingGroceryItemIds = findWithDefault mempty (groceryItemName, groceryItemUnit) existingGroceryItemIdsByNameAndUnit
    Database.updateIngredientGroceryItemIds c userId (setToList existingGroceryItemIds) groceryItemId
  Database.deleteGroceryItems c userId (Map.keys existingGroceryItems)

postUpdateRecipe :: AppM m => Authorization -> UserId -> UpdateRecipeRequest -> m NoContent
postUpdateRecipe token userId UpdateRecipeRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    void $ maybe (throwIO err404) pure . headMay =<< Database.selectRecipes c userId [updateRecipeRequestId]
    Database.updateRecipes c userId [updateRecipeRequestId] updateRecipeRequestActive
    refreshGroceryItems c userId
  pure NoContent

getRecipes :: AppM m => Authorization -> UserId -> m ListRecipeResponse
getRecipes token userId = do
  validateUserToken token userId
  recipes <- unwrapDb $ withDbConn $ \c -> Database.selectRecipes c userId []
  pure ListRecipeResponse
    { listRecipeResponseRecipes = mkReadableRecipe <$> recipes
    }

deleteRecipes :: AppM m => Authorization -> UserId -> DeleteRecipeRequest -> m NoContent
deleteRecipes token userId DeleteRecipeRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    existingIds <- asSet . setFromList . keys <$> Database.selectRecipes c userId (setToList deleteRecipeRequestIds)
    unless (null $ difference deleteRecipeRequestIds existingIds) $ throwIO err400
    Database.deleteRecipes c userId (setToList deleteRecipeRequestIds)
    refreshGroceryItems c userId
  pure NoContent
