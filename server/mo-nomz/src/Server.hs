module Server where

import NomzPrelude

import Chez.Grater (scrapeAndParseUrl)
import Control.Monad.Logger (logError)
import Data.Version (showVersion)
import Network.URI (parseURI)
import Servant.Server (ServerError, err400, err401, err403, err500, errReasonPhrase)
import qualified Data.Text as Text

import API.Types
  ( GetHealthResponse(..), ParseBlobRequest(..), ParseBlobResponse(..), ParseLinkRequest(..)
  , ParseLinkResponse(..), UserCreateResponse(..), UserPingRequest(..), UserPingResponse(..)
  )
import Auth (Authorization, generateToken, validateToken)
import Chez.Grater.Parser (mkIngredients, parseRawIngredients)
import Conversion (mkReadableIngredient, mkReadableStep)
import Foundation (App(..), AppM, appLogFunc, cacheSettings, logErrors, settings, withDbConn)
import Paths_mo_nomz (version)
import Scraper.Site (allScrapers)
import Settings (AppSettings(..), CacheSettings(..))
import Types (OrderedIngredient(..), RecipeLink(..), ScrapedRecipe(..), UserId)
import qualified Database

scrapeUrlCached :: AppM m => RecipeLink -> m ScrapedRecipe
scrapeUrlCached link = do
  app <- ask
  let CacheSettings {..} = cacheSettings app
      manager = appManager app
      scrape = do
        uri <- maybe (throwM err400 { errReasonPhrase = "Invalid link" }) pure $ parseURI (Text.unpack $ unRecipeLink link)
        (name, ingredients, steps, meta) <- scrapeAndParseUrl allScrapers manager uri
        pure (ScrapedRecipe name ingredients steps, meta)
  case cacheSettingsEnabled of
    False -> fst <$> logErrors (liftIO scrape)
    True -> unwrapDb $ withDbConn $ \c -> do
      Database.selectCachedRecipe c link >>= \case
        Just cached -> pure cached
        Nothing -> do
          (cached, meta) <- scrape
          Database.repsertCachedRecipe c link cached meta
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
  App {..} <- ask
  now <- liftIO getCurrentTime
  unwrapDb $ withDbConn $ \c -> do
    Database.health c
    (day, week, month, year) <- Database.selectRecentUsers c
    (cacheSize, mostRecent, leastRecent) <- Database.selectCacheStats c
    pure GetHealthResponse
      { getHealthResponseStatus = "OK"
      , getHealthResponseVersion = Text.pack (showVersion version)
      , getHealthResponseStarted = appStarted
      , getHealthResponseFetched = now
      , getHealthResponseUserDay = day
      , getHealthResponseUserWeek = week
      , getHealthResponseUserMonth = month
      , getHealthResponseUserYear = year
      , getHealthResponseCacheSize = cacheSize
      , getHealthResponseCacheMostRecent = mostRecent
      , getHealthResponseCacheLeastRecent = leastRecent
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
    Database.updateUserPing c userId userPingRequestVersion userPingRequestTarget
  pure $ UserPingResponse "pong"

-- parsing only
postParseBlob :: AppM m => Authorization -> UserId -> ParseBlobRequest -> m ParseBlobResponse
postParseBlob token userId ParseBlobRequest {..} = do
  validateUserToken token userId
  ingredients <- case parseRawIngredients parseBlobRequestContent of
    Left e -> do
      $logError e
      pure $ mkIngredients parseBlobRequestContent
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
    , parseLinkResponseSteps = mkReadableStep <$> scrapedRecipeSteps
    }
