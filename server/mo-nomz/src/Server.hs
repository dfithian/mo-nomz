module Server where

import ClassyPrelude

import Control.Monad.Except (throwError)
import Control.Monad.Logger (logError)
import Database.PostgreSQL.Simple (Connection)
import Network.URI (parseURI)
import Servant.API (NoContent(NoContent))
import Servant.Server (ServerError, err400, err401, err403, err404, err500, errReasonPhrase)

import API.Types
  ( DeleteIngredientRequest(..), DeleteRecipeRequest(..), GetHealthResponse(..)
  , ListIngredientResponse(..), ListRecipeResponse(..), MergeIngredientRequest(..)
  , ReadableIngredient(..), ReadableIngredientAggregate(..), ReadableRecipe(..)
  , RecipeImportBodyRequest(..), RecipeImportLinkRequest(..), UpdateRecipeRequest(..)
  , UserCreateResponse(..)
  )
import Auth (Authorization, generateToken, validateToken)
import Foundation (AppM, settings, withDbConn)
import Scrape (ScrapedRecipe(..), parseIngredients, scrapeUrl)
import Scrub (scrubIngredient)
import Settings (AppSettings(..))
import Types
  ( Ingredient(..), Recipe(..), RecipeIngredient(..), RecipeLink(..), RecipeName(..), UserId
  , mapError
  )
import Unit (mkQuantity, mkReadableQuantity)
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

mkRecipeIngredient :: Ingredient -> RecipeIngredient
mkRecipeIngredient Ingredient {..} = RecipeIngredient
  { recipeIngredientName = ingredientName
  , recipeIngredientQuantity = ingredientQuantity
  , recipeIngredientUnit = ingredientUnit
  }

mkIngredient' :: RecipeIngredient -> Ingredient
mkIngredient' RecipeIngredient {..} = Ingredient
  { ingredientName = recipeIngredientName
  , ingredientQuantity = recipeIngredientQuantity
  , ingredientUnit = recipeIngredientUnit
  , ingredientActive = True
  }
mkReadableIngredient :: Ingredient -> ReadableIngredient
mkReadableIngredient Ingredient {..} = ReadableIngredient
  { readableIngredientName = ingredientName
  , readableIngredientQuantity = mkReadableQuantity ingredientQuantity
  , readableIngredientUnit = ingredientUnit
  , readableIngredientActive = ingredientActive
  }

mkIngredient :: ReadableIngredient -> Ingredient
mkIngredient ReadableIngredient {..} = Ingredient
  { ingredientName = readableIngredientName
  , ingredientQuantity = mkQuantity readableIngredientQuantity
  , ingredientUnit = readableIngredientUnit
  , ingredientActive = readableIngredientActive
  }

mkReadableIngredient' :: RecipeIngredient -> ReadableIngredient
mkReadableIngredient' RecipeIngredient {..} = ReadableIngredient
  { readableIngredientName = recipeIngredientName
  , readableIngredientQuantity = mkReadableQuantity recipeIngredientQuantity
  , readableIngredientUnit = recipeIngredientUnit
  , readableIngredientActive = True
  }

mkReadableRecipe :: Recipe -> ReadableRecipe
mkReadableRecipe Recipe {..} = ReadableRecipe
  { readableRecipeName = recipeName
  , readableRecipeLink = recipeLink
  , readableRecipeActive = recipeActive
  , readableRecipeIngredients = mkReadableIngredient' <$> recipeIngredients
  }
getIngredients :: AppM m => Authorization -> UserId -> m ListIngredientResponse
getIngredients token userId = do
  validateUserToken token userId
  ingredients <- unwrapDb $ withDbConn $ \c -> Database.selectIngredients c userId []
  let readableIngredients = sortOn (readableIngredientName . readableIngredientAggregateIngredient)
        . map (uncurry ReadableIngredientAggregate . first singleton)
        . mapToList . map mkReadableIngredient
        $ ingredients
  pure ListIngredientResponse
    { listIngredientResponseIngredients = readableIngredients
    }

postMergeIngredient :: AppM m => Authorization -> UserId -> MergeIngredientRequest -> m NoContent
postMergeIngredient token userId MergeIngredientRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    existingIds <- asSet . setFromList . keys <$> Database.selectIngredients c userId (setToList mergeIngredientRequestIds)
    unless (null $ difference mergeIngredientRequestIds existingIds) $ throwIO err400
    let newIngredient = Ingredient
          { ingredientName = mergeIngredientRequestName
          , ingredientQuantity = mkQuantity mergeIngredientRequestQuantity
          , ingredientUnit = mergeIngredientRequestUnit
          , ingredientActive = mergeIngredientRequestActive
          }
    Database.mergeIngredients c userId (setToList mergeIngredientRequestIds) newIngredient
  pure NoContent

deleteIngredient :: AppM m => Authorization -> UserId -> DeleteIngredientRequest -> m NoContent
deleteIngredient token userId DeleteIngredientRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    existingIds <- asSet . setFromList . keys <$> Database.selectIngredients c userId (setToList deleteIngredientRequestIds)
    unless (null $ difference deleteIngredientRequestIds existingIds) $ throwIO err400
    Database.deleteIngredients c userId (setToList deleteIngredientRequestIds)
  pure NoContent

postRecipeImportLink :: AppM m => Authorization -> UserId -> RecipeImportLinkRequest -> m NoContent
postRecipeImportLink token userId RecipeImportLinkRequest {..} = do
  validateUserToken token userId
  uri <- maybe (throwError err400 { errReasonPhrase = "Invalid link" }) pure $ parseURI (unpack $ unRecipeLink recipeImportLinkRequestLink)
  ScrapedRecipe {..} <- mapError (\e -> err500 { errReasonPhrase = unpack e }) $ scrapeUrl uri
  rawIngredients <- mapError (\e -> err500 { errReasonPhrase = unpack e }) $ parseIngredients scrapedRecipeContents
  let ingredients = scrubIngredient <$> rawIngredients
      recipe = Recipe
        { recipeName = RecipeName scrapedRecipeTitle
        , recipeLink = Just recipeImportLinkRequestLink
        , recipeIngredients = mkRecipeIngredient <$> ingredients
        , recipeActive = True
        }
  unwrapDb $ withDbConn $ \c -> do
    Database.insertIngredients c userId ingredients
    Database.insertRecipe c userId recipe
  pure NoContent

postRecipeImportBody :: AppM m => Authorization -> UserId -> RecipeImportBodyRequest -> m NoContent
postRecipeImportBody token userId RecipeImportBodyRequest {..} = do
  validateUserToken token userId
  rawIngredients <- mapError (\e -> err500 { errReasonPhrase = unpack e }) $ parseIngredients recipeImportBodyRequestContent
  let ingredients = scrubIngredient <$> rawIngredients
      recipe = Recipe
        { recipeName = recipeImportBodyRequestName
        , recipeLink = Nothing
        , recipeIngredients = mkRecipeIngredient <$> ingredients
        , recipeActive = True
        }
  unwrapDb $ withDbConn $ \c -> do
    Database.insertIngredients c userId ingredients
    Database.insertRecipe c userId recipe
  pure NoContent

refreshIngredients :: Connection -> UserId -> IO ()
refreshIngredients c userId = do
  ingredients <- map mkIngredient' <$> Database.selectActiveIngredients c userId
  Database.deleteIngredients c userId []
  Database.insertIngredients c userId ingredients

postUpdateRecipe :: AppM m => Authorization -> UserId -> UpdateRecipeRequest -> m NoContent
postUpdateRecipe token userId UpdateRecipeRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    void $ maybe (throwIO err404) pure . headMay =<< Database.selectRecipes c userId [updateRecipeRequestId]
    Database.updateRecipe c userId updateRecipeRequestId updateRecipeRequestActive
    refreshIngredients c userId
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
    refreshIngredients c userId
  pure NoContent
