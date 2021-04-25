module Foundation where

import ClassyPrelude hiding (Handler)

import Control.Monad.Except (ExceptT, MonadError, mapExceptT)
import Control.Monad.Logger
  ( Loc, LogLevel, LogSource, LogStr, LoggingT, MonadLoggerIO, askLoggerIO, logError, runLoggingT
  )
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Servant.Server (Handler(Handler), ServerError)

import Settings (AppSettings)

type AppM m = (MonadError ServerError m, MonadIO m, MonadLoggerIO m, MonadReader App m)

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type NomzServer = ExceptT ServerError (ReaderT App (LoggingT IO))

data App = App
  { appSettings       :: AppSettings -- ^ The settings for the app.
  , appConnectionPool :: Pool Connection -- ^ The database connection pool.
  , appLogFunc        :: LogFunc -- ^ The logging function.
  }

class HasDatabase a where
  connectionPool :: a -> Pool Connection

instance HasDatabase App where
  connectionPool = appConnectionPool

instance HasDatabase (Pool Connection) where
  connectionPool = id

class HasSettings a where
  settings :: a -> AppSettings

instance HasSettings App where
  settings = appSettings

instance HasSettings AppSettings where
  settings = id

withDbConn :: (HasDatabase r, MonadIO m, MonadLoggerIO m, MonadReader r m) => (Connection -> IO a) -> m (Either SomeException a)
withDbConn f = do
  pool <- asks connectionPool
  logFunc <- askLoggerIO
  liftIO $ withResource pool $ \c -> (Right <$> withTransaction c (f c)) `catch` \se -> do
    runLoggingT ($logError (tshow se)) logFunc
    pure $ Left se

runNomzServer :: App -> NomzServer a -> Handler a
runNomzServer app ma = Handler (mapExceptT (\ma' -> runLoggingT (runReaderT ma' app) (appLogFunc app)) ma)
