module Server where

import ClassyPrelude

import Control.Monad.Except (MonadError, throwError)
import Network.URI (parseURI)
import Servant.API (NoContent(NoContent))
import Servant.Server (ServerError, err400, err404, err500, errReasonPhrase)

import API.Types
  ( DeleteIngredientRequest(..), ListIngredientResponse(..), MergeIngredientRequest(..)
  , ReadableIngredient(..), ReadableIngredientAggregate(..), RecipeImportLinkRequest(..)
  , UserCreateRequest(..), UserCreateResponse(..)
  )
import Foundation (HasDatabase, withDbConn)
import Scrape (parseIngredients, scrapeUrl)
import Scrub (scrubIngredient)
import Types (Ingredient(..), RecipeLink(..), UserId, mapError)
import Unit (mkQuantity, mkReadableQuantity)
import qualified Database

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

postRecipeImportLink :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> RecipeImportLinkRequest -> m NoContent
postRecipeImportLink userId RecipeImportLinkRequest {..} = do
  ensureUserExists userId
  uri <- maybe (throwError err400 { errReasonPhrase = "Invalid link" }) pure $ parseURI (unpack $ unRecipeLink recipeImportLinkRequestLink)
  rawIngredients <- mapError (\e -> err500 { errReasonPhrase = unpack e }) $ parseIngredients =<< scrapeUrl uri
  let ingredients = scrubIngredient <$> rawIngredients
  withDbConn $ \c -> Database.insertIngredients c userId ingredients
  pure NoContent

mkReadableIngredient :: Ingredient -> ReadableIngredient
mkReadableIngredient Ingredient {..} = ReadableIngredient
  { readableIngredientName = ingredientName
  , readableIngredientQuantity = mkReadableQuantity ingredientQuantity
  , readableIngredientUnit = ingredientUnit
  }

mkIngredient :: ReadableIngredient -> Ingredient
mkIngredient ReadableIngredient {..} = Ingredient
  { ingredientName = readableIngredientName
  , ingredientQuantity = mkQuantity readableIngredientQuantity
  , ingredientUnit = readableIngredientUnit
  }

getIngredients :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> m ListIngredientResponse
getIngredients userId = do
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
  existingIds <- asSet . setFromList . keys <$> withDbConn (\c -> Database.selectIngredients c userId (setToList mergeIngredientRequestIds))
  unless (null $ difference mergeIngredientRequestIds existingIds) $
    throwError err400
  let newIngredient = Ingredient
        { ingredientName = mergeIngredientRequestName
        , ingredientQuantity = mkQuantity mergeIngredientRequestQuantity
        , ingredientUnit = mergeIngredientRequestUnit
        }
  withDbConn $ \c -> Database.mergeIngredients c userId (setToList mergeIngredientRequestIds) newIngredient
  pure NoContent

deleteIngredient :: (HasDatabase r, MonadError ServerError m, MonadIO m, MonadReader r m) => UserId -> DeleteIngredientRequest -> m NoContent
deleteIngredient userId DeleteIngredientRequest {..} = do
  existingIds <- asSet . setFromList . keys <$> withDbConn (\c -> Database.selectIngredients c userId (setToList deleteIngredientRequestIds))
  unless (null $ difference deleteIngredientRequestIds existingIds) $
    throwError err400
  withDbConn $ \c -> Database.deleteIngredients c userId (setToList deleteIngredientRequestIds)
  pure NoContent
