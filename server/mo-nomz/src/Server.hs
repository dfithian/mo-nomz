module Server where

import ClassyPrelude

import Control.Monad.Except (MonadError, throwError)
import Network.URI (parseURI, uriAuthority, uriPath, uriRegName)
import Servant.API (NoContent(NoContent))
import Servant.Server (ServerError, err400, errReasonPhrase)

import API.Types
  ( ListIngredientResponse(..), ListRecipeResponse(..), RecipeImportBodyRequest(..)
  , RecipeImportLinkRequest(..), RecipeImportResponse(..)
  )
import Foundation (HasDatabase, withDbConn)
import Types (Ingredient(..), RecipeLink(..), RecipeName(..), RecipeId)
import Unit (combineQuantities)
import qualified Database

postRecipeImportLink :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => RecipeImportLinkRequest -> m RecipeImportResponse
postRecipeImportLink RecipeImportLinkRequest {..} = do
  let recipeNameMay = do
        uri <- parseURI (unpack recipeImportLinkRequestLink)
        uriAuth <- uriAuthority uri
        pure . RecipeName . pack $ uriRegName uriAuth <> uriPath uri
  recipeName <- maybe (throwError err400 { errReasonPhrase = "Invalid domain" }) pure recipeNameMay
  recipeId <- withDbConn $ \c -> Database.insertRecipe c recipeName (Just $ RecipeLink recipeImportLinkRequestLink) mempty
  pure RecipeImportResponse
    { recipeImportResponseId = recipeId
    }

postRecipeImportBody :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => RecipeImportBodyRequest -> m RecipeImportResponse
postRecipeImportBody RecipeImportBodyRequest {..} = do
  recipeId <- withDbConn $ \c -> Database.insertRecipe c recipeImportBodyRequestName Nothing recipeImportBodyRequestIngredients
  pure RecipeImportResponse
    { recipeImportResponseId = recipeId
    }

getRecipes :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => [RecipeId] -> m ListRecipeResponse
getRecipes recipeIds = do
  recipes <- withDbConn $ \c -> Database.selectRecipeIngredients c recipeIds
  pure ListRecipeResponse
    { listRecipeResponseRecipes = recipes
    }

deleteRecipe :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => RecipeId -> m NoContent
deleteRecipe recipeId = do
  withDbConn $ \c -> Database.deleteRecipe c recipeId
  pure NoContent

getIngredients :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => [RecipeId] -> m ListIngredientResponse
getIngredients recipeIds = do
  ingredients <- withDbConn $ \c -> Database.selectIngredients c recipeIds
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
