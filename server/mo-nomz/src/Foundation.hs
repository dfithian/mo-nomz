module Foundation where

import NomzPrelude

import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, logError)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Network.HTTP.Client (Manager, managerModifyRequest, requestHeaders)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types (hUserAgent)
import Servant.Server (Handler(Handler), ServerError)

import Settings (AppSettings)

type AppM m = (MonadCatch m, MonadError ServerError m, MonadIO m, MonadLoggerIO m, MonadReader App m)

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type NomzServer = ExceptT ServerError (ReaderT App (LoggingT IO))

data App = App
  { appSettings       :: AppSettings -- ^ The settings for the app.
  , appConnectionPool :: Pool Connection -- ^ The database connection pool.
  , appLogFunc        :: LogFunc -- ^ The logging function.
  , appManager        :: Manager -- ^ The manager for our scrape client.
  , appStarted        :: UTCTime -- ^ The time the app was started.
  }

class HasDatabase a where
  connectionPool :: a -> Pool Connection

instance HasDatabase App where
  connectionPool = appConnectionPool

instance HasDatabase (Pool Connection) where
  connectionPool = id

class HasManager a where
  manager :: a -> Manager

instance HasManager App where
  manager = appManager

instance HasManager Manager where
  manager = id

class HasSettings a where
  settings :: a -> AppSettings

instance HasSettings App where
  settings = appSettings

instance HasSettings AppSettings where
  settings = id

logErrors :: (MonadCatch m, MonadLoggerIO m) => m a -> m a
logErrors ma = do
  logFunc <- askLoggerIO
  ma `catch` \(se :: SomeException) -> do
    runLoggingT ($logError (tshow se)) logFunc
    throwM se

withDbConn :: (HasDatabase r, MonadIO m, MonadLoggerIO m, MonadReader r m) => (Connection -> IO a) -> m (Either SomeException a)
withDbConn f = do
  pool <- asks connectionPool
  logFunc <- askLoggerIO
  liftIO $ withResource pool $ \c -> (Right <$> withTransaction c (f c)) `catch` \se -> do
    runLoggingT ($logError (tshow se)) logFunc
    pure $ Left se

runNomzServer :: App -> NomzServer a -> Handler a
runNomzServer app ma = Handler (mapExceptT (\ma' -> runLoggingT (runReaderT ma' app) (appLogFunc app)) ma)

userAgent :: ByteString
userAgent = "Simulated"

createManager :: IO Manager
createManager =
  newTlsManagerWith tlsManagerSettings
    { managerModifyRequest = \req -> do
        pure req
          { requestHeaders = [(hUserAgent, userAgent)] <> requestHeaders req
          }
    }
