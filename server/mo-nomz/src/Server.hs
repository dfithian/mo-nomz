module Server where

import ClassyPrelude

import Control.Monad.Except (MonadError, throwError)
import Network.URI (parseURI, uriAuthority, uriPath, uriRegName)
import Servant.API (NoContent(NoContent))
import Servant.Server (ServerError, err400, err404, err500, errReasonPhrase)

import API.Types
  ( ListIngredientResponse(..), ListRecipeResponse(..), ListUserResponse(..)
  , RecipeImportBodyRequest(..), RecipeImportLinkRequest(..), RecipeImportResponse(..)
  , UpdateRecipeRequest(..), UserCreateRequest(..), UserCreateResponse(..)
  )
import Foundation (HasDatabase, withDbConn)
import Scrape (parseIngredients, scrapeUrl)
import Scrub (scrubIngredient)
import Types (Ingredient(..), Recipe(..), RecipeLink(..), RecipeName(..), RecipeId, UserId, mapError)
import Unit (combineQuantities)
import qualified Database

getUsers :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => m ListUserResponse
getUsers = do
  users <- withDbConn $ \c -> Database.selectUsersByUsername c []
  pure $ ListUserResponse users

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

postRecipeImportLink :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> RecipeImportLinkRequest -> m RecipeImportResponse
postRecipeImportLink userId RecipeImportLinkRequest {..} = do
  ensureUserExists userId
  uri <- maybe (throwError err400 { errReasonPhrase = "Invalid link" }) pure $ parseURI (unpack $ unRecipeLink recipeImportLinkRequestLink)
  let recipeNameMay = do
        uriAuth <- uriAuthority uri
        pure . RecipeName . pack $ uriRegName uriAuth <> uriPath uri
  recipeName <- maybe (throwError err400 { errReasonPhrase = "Invalid domain" }) pure recipeNameMay
  rawIngredients <- mapError (\e -> err500 { errReasonPhrase = unpack e }) $ parseIngredients =<< scrapeUrl uri
  let ingredients = scrubIngredient <$> rawIngredients
      recipe = Recipe
        { recipeName = recipeName
        , recipeIngredients = ingredients
        , recipeLink = Just recipeImportLinkRequestLink
        }
  recipeId <- withDbConn $ \c -> Database.insertRecipe c userId recipe
  pure RecipeImportResponse
    { recipeImportResponseId = recipeId
    }

postRecipeImportBody :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> RecipeImportBodyRequest -> m RecipeImportResponse
postRecipeImportBody userId RecipeImportBodyRequest {..} = do
  ensureUserExists userId
  let recipe = Recipe
        { recipeName = recipeImportBodyRequestName
        , recipeIngredients = recipeImportBodyRequestIngredients
        , recipeLink = Nothing
        }
  recipeId <- withDbConn $ \c -> Database.insertRecipe c userId recipe
  pure RecipeImportResponse
    { recipeImportResponseId = recipeId
    }

postUpdateRecipe :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> RecipeId -> UpdateRecipeRequest -> m NoContent
postUpdateRecipe userId recipeId UpdateRecipeRequest {..} = do
  recipe <- maybe (throwError err404) pure . lookup recipeId
    =<< withDbConn (\c -> Database.selectRecipes c userId [recipeId])
  withDbConn $ \c -> Database.updateRecipe c recipeId recipe { recipeIngredients = updateRecipeRequestIngredients }
  pure NoContent

getRecipes :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> [RecipeId] -> m ListRecipeResponse
getRecipes userId recipeIds = do
  ensureUserExists userId
  recipes <- withDbConn $ \c -> Database.selectRecipes c userId recipeIds
  pure ListRecipeResponse
    { listRecipeResponseRecipes = recipes
    }

deleteRecipe :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> RecipeId -> m NoContent
deleteRecipe userId recipeId = do
  ensureUserExists userId
  withDbConn $ \c -> Database.deleteRecipe c userId recipeId
  pure NoContent

getIngredients :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> [RecipeId] -> m ListIngredientResponse
getIngredients userId recipeIds = do
  ingredients <- withDbConn $ \c -> Database.selectIngredients c userId recipeIds
  let combinedIngredients =
        mconcat
          . map (\(name, unitsAndQuantities) -> uncurry (flip (Ingredient name)) <$> unitsAndQuantities)
          . mapToList
          . map (mapToList . combineQuantities . map sum . foldr (\(nextUnit, nextQuantity) acc -> asMap $ insertWith (<>) nextUnit [nextQuantity] acc) mempty)
          . foldr (\Ingredient {..} acc -> asMap $ insertWith (<>) ingredientName [(ingredientUnit, ingredientQuantity)] acc) mempty
          $ ingredients
  pure ListIngredientResponse
    { listIngredientResponseIngredients = combinedIngredients
    }
