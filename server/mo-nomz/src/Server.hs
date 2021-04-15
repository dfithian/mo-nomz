module Server where

import ClassyPrelude

import Control.Monad.Except (MonadError, throwError)
import Database.PostgreSQL.Simple (withTransaction)
import Network.URI (parseURI)
import Servant.API (NoContent(NoContent))
import Servant.Server (ServerError, err400, err404, err500, errReasonPhrase)

import API.Types
  ( DeleteIngredientRequest(..), DeleteRecipeRequest(..), GetHealthResponse(..)
  , ListIngredientResponse(..), ListRecipeResponse(..), MergeIngredientRequest(..)
  , ReadableIngredient(..), ReadableIngredientAggregate(..), ReadableRecipe(..)
  , RecipeImportBodyRequest(..), RecipeImportLinkRequest(..), UpdateRecipeRequest(..)
  , UserCreateRequest(..), UserCreateResponse(..)
  )
import Foundation (HasDatabase, withDbConn)
import Scrape (ScrapedRecipe(..), parseIngredients, scrapeUrl)
import Scrub (scrubIngredient)
import Types
  ( Ingredient(..), Recipe(..), RecipeIngredient(..), RecipeLink(..), RecipeName(..), UserId
  , mapError
  )
import Unit (mkQuantity, mkReadableQuantity)
import qualified Database

getHealth :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => m GetHealthResponse
getHealth = do
  withDbConn Database.health
  pure GetHealthResponse
    { getHealthResponseStatus = "ok"
    }

postCreateUser :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserCreateRequest -> m UserCreateResponse
postCreateUser UserCreateRequest {..} = do
  userId <- withDbConn $ \c ->
    Database.fetchUserIdByUsername c userCreateRequestUsername >>= \case
      Just userId -> pure userId
      Nothing -> Database.insertUser c userCreateRequestUsername
  pure $ UserCreateResponse userId

ensureUserExists :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> m ()
ensureUserExists userId = do
  withDbConn (\c -> Database.fetchUserExists c userId) >>= \case
    True -> pure ()
    False -> throwError err404 { errReasonPhrase = "User does not exist" }

mkReadableIngredient :: Ingredient -> ReadableIngredient
mkReadableIngredient Ingredient {..} = ReadableIngredient
  { readableIngredientName = ingredientName
  , readableIngredientQuantity = mkReadableQuantity ingredientQuantity
  , readableIngredientUnit = ingredientUnit
  , readableIngredientActive = ingredientActive
  }

mkRecipeIngredient :: Ingredient -> RecipeIngredient
mkRecipeIngredient Ingredient {..} = RecipeIngredient
  { recipeIngredientName = ingredientName
  , recipeIngredientQuantity = ingredientQuantity
  , recipeIngredientUnit = ingredientUnit
  }

mkIngredient :: ReadableIngredient -> Ingredient
mkIngredient ReadableIngredient {..} = Ingredient
  { ingredientName = readableIngredientName
  , ingredientQuantity = mkQuantity readableIngredientQuantity
  , ingredientUnit = readableIngredientUnit
  , ingredientActive = readableIngredientActive
  }

mkIngredient' :: RecipeIngredient -> Ingredient
mkIngredient' RecipeIngredient {..} = Ingredient
  { ingredientName = recipeIngredientName
  , ingredientQuantity = recipeIngredientQuantity
  , ingredientUnit = recipeIngredientUnit
  , ingredientActive = True
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

getIngredients :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> m ListIngredientResponse
getIngredients userId = do
  ensureUserExists userId
  ingredients <- withDbConn $ \c -> Database.selectIngredients c userId []
  let readableIngredients = sortOn (readableIngredientName . readableIngredientAggregateIngredient)
        . map (uncurry ReadableIngredientAggregate . first singleton)
        . mapToList . map mkReadableIngredient
        $ ingredients
  pure ListIngredientResponse
    { listIngredientResponseIngredients = readableIngredients
    }

postMergeIngredient :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> MergeIngredientRequest -> m NoContent
postMergeIngredient userId MergeIngredientRequest {..} = do
  ensureUserExists userId
  existingIds <- asSet . setFromList . keys <$> withDbConn (\c -> Database.selectIngredients c userId (setToList mergeIngredientRequestIds))
  unless (null $ difference mergeIngredientRequestIds existingIds) $
    throwError err400
  let newIngredient = Ingredient
        { ingredientName = mergeIngredientRequestName
        , ingredientQuantity = mkQuantity mergeIngredientRequestQuantity
        , ingredientUnit = mergeIngredientRequestUnit
        , ingredientActive = mergeIngredientRequestActive
        }
  withDbConn $ \c -> withTransaction c $ Database.mergeIngredients c userId (setToList mergeIngredientRequestIds) newIngredient
  pure NoContent

deleteIngredient :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> DeleteIngredientRequest -> m NoContent
deleteIngredient userId DeleteIngredientRequest {..} = do
  ensureUserExists userId
  existingIds <- asSet . setFromList . keys <$> withDbConn (\c -> Database.selectIngredients c userId (setToList deleteIngredientRequestIds))
  unless (null $ difference deleteIngredientRequestIds existingIds) $
    throwError err400
  withDbConn $ \c -> Database.deleteIngredients c userId (setToList deleteIngredientRequestIds)
  pure NoContent

postRecipeImportLink :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> RecipeImportLinkRequest -> m NoContent
postRecipeImportLink userId RecipeImportLinkRequest {..} = do
  ensureUserExists userId
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
  withDbConn $ \c -> withTransaction c $ do
    Database.insertIngredients c userId ingredients
    Database.insertRecipe c userId recipe
  pure NoContent

postRecipeImportBody :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> RecipeImportBodyRequest -> m NoContent
postRecipeImportBody userId RecipeImportBodyRequest {..} = do
  ensureUserExists userId
  rawIngredients <- mapError (\e -> err500 { errReasonPhrase = unpack e }) $ parseIngredients recipeImportBodyRequestContent
  let ingredients = scrubIngredient <$> rawIngredients
      recipe = Recipe
        { recipeName = recipeImportBodyRequestName
        , recipeLink = Nothing
        , recipeIngredients = mkRecipeIngredient <$> ingredients
        , recipeActive = True
        }
  withDbConn $ \c -> withTransaction c $ do
    Database.insertIngredients c userId ingredients
    Database.insertRecipe c userId recipe
  pure NoContent

refreshIngredients :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> m ()
refreshIngredients userId = do
  ingredients <- map mkIngredient' <$> withDbConn (\c -> Database.selectActiveIngredients c userId)
  withDbConn $ \c -> withTransaction c $ do
    Database.deleteIngredients c userId []
    Database.insertIngredients c userId ingredients

postUpdateRecipe :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> UpdateRecipeRequest -> m NoContent
postUpdateRecipe userId UpdateRecipeRequest {..} = do
  ensureUserExists userId
  void $ maybe (throwError err404) pure . headMay =<< withDbConn (\c -> Database.selectRecipes c userId [updateRecipeRequestId])
  withDbConn $ \c -> Database.updateRecipe c userId updateRecipeRequestId updateRecipeRequestActive
  refreshIngredients userId
  pure NoContent

getRecipes :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> m ListRecipeResponse
getRecipes userId = do
  ensureUserExists userId
  recipes <- withDbConn $ \c -> Database.selectRecipes c userId []
  pure ListRecipeResponse
    { listRecipeResponseRecipes = mkReadableRecipe <$> recipes
    }

deleteRecipes :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> DeleteRecipeRequest -> m NoContent
deleteRecipes userId DeleteRecipeRequest {..} = do
  ensureUserExists userId
  existingIds <- asSet . setFromList . keys <$> withDbConn (\c -> Database.selectRecipes c userId (setToList deleteRecipeRequestIds))
  unless (null $ difference deleteRecipeRequestIds existingIds) $
    throwError err400
  withDbConn $ \c -> withTransaction c $ Database.deleteRecipes c userId (setToList deleteRecipeRequestIds)
  refreshIngredients userId
  pure NoContent
