module Cache where

import ClassyPrelude hiding (link)
import Database.PostgreSQL.Simple (Connection)

import Scraper.Internal.Types (ScrapedRecipe)
import Settings (CacheSettings(..))
import Types (RecipeLink)
import qualified Database

withCachedRecipe :: MonadIO m => CacheSettings -> Connection -> RecipeLink -> m ScrapedRecipe -> (ScrapedRecipe -> m a) -> m a
withCachedRecipe CacheSettings {..} conn link mkCached cont = do
  liftIO (Database.selectCachedRecipe conn link cacheSettingsValidSeconds) >>= \case
    Just cached -> cont cached
    Nothing -> do
      cached <- mkCached
      liftIO $ Database.repsertCachedRecipe conn link cached
      cont cached
