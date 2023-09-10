module Foundation where

import Prelude

import Chez.Server.Context (ChezContext)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (ExceptT, MonadError, mapExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Text (Text, pack)
import Servant.Server (Handler(Handler), ServerError)

import Settings (AppSettings)

type AppM m = (MonadCatch m, MonadError ServerError m, MonadIO m, MonadLogger m, MonadReader App m)

type NomzServer = ExceptT ServerError (ReaderT App (LoggingT IO))

data App = App
  { appSettings    :: AppSettings -- ^ The settings for the app.
  , appChezContext :: ChezContext
  }

class HasSettings a where
  settings :: a -> AppSettings

instance HasSettings App where
  settings = appSettings

instance HasSettings AppSettings where
  settings = id

tshow :: Show a => a -> Text
tshow = pack . show

runNomzServer :: App -> NomzServer a -> Handler a
runNomzServer app ma = Handler (mapExceptT (\ma' -> runStdoutLoggingT (runReaderT ma' app)) ma)
