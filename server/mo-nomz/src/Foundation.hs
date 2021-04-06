module Foundation where

import ClassyPrelude hiding (Handler)

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, LoggingT, runLoggingT)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import Servant.Server (Handler(Handler), ServerError)

import Settings (AppSettings)

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type NomzServer = ExceptT ServerError (ReaderT App (LoggingT IO))

data App = App
  { appSettings       :: AppSettings -- ^ The settings for the app.
  , appConnectionPool :: Pool Connection -- ^ The database connection pool.
  , appLogFunc        :: LogFunc -- ^ The logging function.
  }

class HasDatabase a where
  connectionPool :: a -> (Pool Connection)

instance HasDatabase App where
  connectionPool = appConnectionPool

withDbConn :: (HasDatabase r, MonadIO m, MonadReader r m) => (Connection -> IO a) -> m a
withDbConn f = do
  pool <- asks connectionPool
  liftIO $ withResource pool f

runNomzServer :: App -> NomzServer a -> Handler a
runNomzServer app ma = Handler (mapExceptT (\ma' -> runLoggingT (runReaderT ma' app) (appLogFunc app)) ma)
