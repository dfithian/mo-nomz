module Foundation where

import Prelude

import Control.Monad.Catch (MonadCatch, SomeException, catch, throwM)
import Control.Monad.Except (ExceptT, MonadError, mapExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger
  ( Loc, LogLevel, LogSource, LogStr, LoggingT, MonadLogger, logError, runStdoutLoggingT
  )
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Text (Text, pack)
import Network.HTTP.Client (Manager)
import Servant.Server (Handler(Handler), ServerError)

import Settings (AppSettings)

type AppM m = (MonadCatch m, MonadError ServerError m, MonadIO m, MonadLogger m, MonadReader App m)

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type NomzServer = ExceptT ServerError (ReaderT App (LoggingT IO))

data App = App
  { appSettings :: AppSettings -- ^ The settings for the app.
  , appManager  :: Manager
  }

class HasSettings a where
  settings :: a -> AppSettings

instance HasSettings App where
  settings = appSettings

instance HasSettings AppSettings where
  settings = id

tshow :: Show a => a -> Text
tshow = pack . show

logErrors :: (MonadCatch m, MonadIO m) => m a -> m a
logErrors ma = do
  ma `catch` \(se :: SomeException) -> do
    runStdoutLoggingT ($logError (tshow se))
    throwM se

runNomzServer :: App -> NomzServer a -> Handler a
runNomzServer app ma = Handler (mapExceptT (\ma' -> runStdoutLoggingT (runReaderT ma' app)) ma)
