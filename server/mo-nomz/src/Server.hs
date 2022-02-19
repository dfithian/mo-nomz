module Server where

import Prelude

import Control.Exception (SomeException, fromException, throwIO)
import Control.Monad (unless, void, when)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logError, runLoggingT)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Data.Version (showVersion)
import Network.HTTP.Types (hLocation)
import Network.URI (parseURI)
import Servant.API (NoContent(NoContent))
import Servant.Server
  ( ServerError, err307, err400, err401, err403, err404, err500, errHeaders, errReasonPhrase
  )
import Text.Blaze ((!), Markup, toValue)
import Web.UAParser (OSResult(..), parseOS)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlAttr

import API.Types
  ( DeleteGroceryItemRequest(..), DeleteRecipeRequest(..), ExportGroceryItem(..)
  , ExportIngredient(..), ExportRecipe(..), ExportResponse(..), GetHealthResponse(..)
  , GroceryImportBlobRequest(..), ListGroceryItemResponse(..), ListRecipeResponse(..)
  , ListRecipeResponseV1(..), MergeGroceryItemRequest(..), ParseBlobRequest(..)
  , ParseBlobResponse(..), ParseLinkRequest(..), ParseLinkResponse(..), RecipeImportLinkRequest(..)
  , UpdateGroceryItemRequest(..), UpdateRecipeIngredientsRequest(..), UpdateRecipeRequest(..)
  , UserCreateResponse(..), UserPingRequest(..), UserPingResponse(..), ReadableRecipe
  )
import Auth (Authorization, generateToken, validateToken)
import Conversion
  ( mkOrderedIngredient, mkQuantity, mkReadableGroceryItem, mkReadableIngredient, mkReadableQuantity
  , mkReadableRecipe, mkReadableRecipeV1, mkReadableUnit, mkUnit
  )
import Foundation (App(..), AppM, appLogFunc, cacheSettings, settings, withDbConn)
import Parser (parseRawIngredients, unparseRawIngredients)
import Paths_mo_nomz (version)
import Scrape (scrapeUrl)
import Scraper.Types (ScrapedRecipe(..))
import Settings (AppSettings(..), CacheSettings(..))
import Types
  ( GroceryItem(..), Ingredient(..), OrderedGroceryItem(..), OrderedIngredient(..), Recipe(..)
  , RecipeLink(..), RecipeId, UserId, ingredientToGroceryItem
  )
import Utils (headMay, tshow)
import qualified Database

scrapeUrlCached :: AppM m => RecipeLink -> m ScrapedRecipe
scrapeUrlCached link = do
  app <- ask
  let CacheSettings {..} = cacheSettings app
      scrape = do
        uri <- maybe (throwIO err400 { errReasonPhrase = "Invalid link" }) pure $ parseURI (Text.unpack $ unRecipeLink link)
        recipeWithInfo@(ScrapedRecipe {..}, _) <- either (\e -> throwIO err500 { errReasonPhrase = Text.unpack e }) pure
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

getMetrics :: AppM m => m Markup
getMetrics = do
  let renderMetric (key, value) = Html.div (Html.span (Html.toHtml (Text.unwords [key, tshow value])))
  App {..} <- ask
  now <- liftIO getCurrentTime
  (dayUsers, weekUsers, monthUsers, yearUsers) <- unwrapDb $ withDbConn Database.selectRecentUsers
  healthHtml <- Html.div (Html.span (Html.text "Health OK")) <$ getHealth
  let uptimeHtml = Html.div (Html.span (Html.text $ "Started at " <> Text.pack (formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") appStarted <> " UTC")))
      refreshHtml = Html.div (Html.span (Html.text $ "Last refreshed at " <> Text.pack (formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") now <> " UTC")))
      versionHtml = Html.div (Html.span (Html.text $ "Version " <> Text.pack (showVersion version)))
      metricsHtml = mconcat . fmap renderMetric $
        [ ("recent_users_day", dayUsers)
        , ("recent_users_week", weekUsers)
        , ("recent_users_month", monthUsers)
        , ("recent_users_year", yearUsers)
        ]
  pure $ Html.html $ do
    Html.head $ do
      Html.meta ! HtmlAttr.httpEquiv "Refresh" ! HtmlAttr.content "300"
      Html.style $ Html.text "span { font-family: Courier New; font-size: 14px; }"
    Html.body $ mconcat [uptimeHtml, refreshHtml, versionHtml, healthHtml, metricsHtml]

embedRecipe :: AppM m => Maybe Text -> Maybe RecipeLink -> m Markup
embedRecipe userAgentMay linkMay = case (parseOS =<< fmap Text.encodeUtf8 userAgentMay, linkMay) of
  (Just OSResult {..}, Just (RecipeLink link)) | osrFamily == "iOS" ->
    pure $ Html.html $ do
      Html.head $ do
        Html.style $ Html.text "body,html{width:100%;height:100%;overflow:hidden}iframe{width:100%;height:100%;border:none}"
      Html.body $ do
        Html.iframe ! HtmlAttr.src (toValue link) ! HtmlAttr.height "100%" $ pure ()
  (_, Just (RecipeLink link)) -> throwError err307
    { errHeaders = [(hLocation, Text.encodeUtf8 link)]
    }
  _ -> throwError err307
    { errHeaders = [(hLocation, "/")]
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
        $logError $ "User token validation failed for " <> tshow userId <> " due to " <> Text.pack err
        throwError err403
    _ -> do
      $logError $ "No user token for " <> tshow userId
      throwError err403

postPingUser :: AppM m => Authorization -> UserId -> UserPingRequest -> m UserPingResponse
postPingUser token userId UserPingRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c ->
    Database.updateUserPing c userId userPingRequestVersion
  pure $ UserPingResponse "pong"

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
    existingIds <- Set.fromList . Map.keys <$> Database.selectGroceryItems c userId (Set.toList mergeGroceryItemRequestIds)
    unless (Set.null $ Set.difference mergeGroceryItemRequestIds existingIds) $ throwIO err400
    let newGroceryItem = OrderedGroceryItem
          { orderedGroceryItemItem = GroceryItem
            { groceryItemName = mergeGroceryItemRequestName
            , groceryItemQuantity = mkQuantity mergeGroceryItemRequestQuantity
            , groceryItemUnit = mkUnit mergeGroceryItemRequestUnit
            , groceryItemActive = mergeGroceryItemRequestActive
            }
          , orderedGroceryItemOrder = mergeGroceryItemRequestOrder
          }
    void $ Database.mergeGroceryItems c userId (Set.toList mergeGroceryItemRequestIds) newGroceryItem
  pure NoContent

deleteGroceryItem :: AppM m => Authorization -> UserId -> DeleteGroceryItemRequest -> m NoContent
deleteGroceryItem token userId DeleteGroceryItemRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    existingIds <- Set.fromList . Map.keys <$> Database.selectGroceryItems c userId (Set.toList deleteGroceryItemRequestIds)
    unless (Set.null $ Set.difference deleteGroceryItemRequestIds existingIds) $ throwIO err400
    Database.deleteGroceryItems c userId (Set.toList deleteGroceryItemRequestIds)
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
  case headMay (Map.toList existingRecipes) of
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
  ingredients <- either (\e -> throwError err500 { errReasonPhrase = Text.unpack e }) pure $ parseRawIngredients groceryImportBlobRequestContent
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
    Recipe {..} <- maybe (throwIO err404) (pure . snd) . headMay . Map.toList =<< Database.selectRecipes c userId [updateRecipeRequestId]
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
    let existingIds = Set.fromList . Map.keys $ existing
        deleteIngredientIds = Set.toList updateRecipeIngredientsRequestDeletes
    Recipe {..} <- maybe (throwIO err400) pure $ Map.lookup updateRecipeIngredientsRequestId existing
    unless (Set.null $ Set.difference (Set.singleton updateRecipeIngredientsRequestId) existingIds) $ throwIO err400
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
    { listRecipeResponseV1Recipes = Map.fromList
        . fmap (\(recipeId, recipe) -> (recipeId, mkReadableRecipeV1 (orderedIngredientIngredient <$> Map.elems (Map.findWithDefault mempty recipeId ingredients)) recipe))
        . Map.toList
        $ recipes
    }

getRecipes :: AppM m => Authorization -> UserId -> m ListRecipeResponse
getRecipes token userId = do
  validateUserToken token userId
  (recipes, ingredients) <- unwrapDb $ withDbConn $ \c -> (,)
    <$> Database.selectRecipes c userId []
    <*> Database.selectIngredientsByRecipeIds c userId []
  pure ListRecipeResponse
    { listRecipeResponseRecipes = Map.fromList
        . fmap (\(recipeId, recipe) -> (recipeId, mkReadableRecipe (Map.findWithDefault mempty recipeId ingredients) recipe))
        . Map.toList
        $ recipes
    }

getRecipe :: AppM m => Authorization -> UserId -> RecipeId -> m ReadableRecipe
getRecipe token userId recipeId = do
  validateUserToken token userId
  (recipes, ingredients) <- unwrapDb $ withDbConn $ \c -> (,)
    <$> Database.selectRecipes c userId [recipeId]
    <*> Database.selectIngredientsByRecipeId c userId recipeId
  case headMay (Map.toList recipes) of
    Nothing -> throwError err404
    Just (_, recipe) -> pure $ mkReadableRecipe ingredients recipe

deleteRecipes :: AppM m => Authorization -> UserId -> DeleteRecipeRequest -> m NoContent
deleteRecipes token userId DeleteRecipeRequest {..} = do
  validateUserToken token userId
  unwrapDb $ withDbConn $ \c -> do
    existingIds <- Set.fromList . Map.keys <$> Database.selectRecipes c userId (Set.toList deleteRecipeRequestIds)
    unless (Set.null $ Set.difference deleteRecipeRequestIds existingIds) $ throwIO err400
    ingredientIds <- Database.selectRecipeIngredientIds c userId (Set.toList deleteRecipeRequestIds)
    Database.unmergeGroceryItems c userId ingredientIds
    Database.deleteRecipes c userId (Set.toList deleteRecipeRequestIds)
  pure NoContent

-- parsing only
postParseBlob :: AppM m => Authorization -> UserId -> ParseBlobRequest -> m ParseBlobResponse
postParseBlob token userId ParseBlobRequest {..} = do
  validateUserToken token userId
  ingredients <- case parseRawIngredients parseBlobRequestContent of
    Left e -> do
      $logError e
      pure $ unparseRawIngredients parseBlobRequestContent
    Right is -> pure is
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
    { exportResponseGroceries = fmap (\OrderedGroceryItem {..} ->
        let GroceryItem {..} = orderedGroceryItemItem
        in ExportGroceryItem
          { exportGroceryItemName = groceryItemName
          , exportGroceryItemQuantity = mkReadableQuantity groceryItemQuantity
          , exportGroceryItemUnit = mkReadableUnit groceryItemUnit
          , exportGroceryItemActive = groceryItemActive
          , exportGroceryItemOrder = orderedGroceryItemOrder
          }
      ) groceries
    , exportResponseRecipes = fmap (\Recipe {..} ->
        ExportRecipe
          { exportRecipeName = recipeName
          , exportRecipeLink = recipeLink
          , exportRecipeActive = recipeActive
          , exportRecipeRating = recipeRating
          , exportRecipeNotes = recipeNotes
          }
      ) recipes
    , exportResponseIngredients = fmap (\(recipeId, groceryItemId, OrderedIngredient {..}) ->
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
