module TestEnv where

import ClassyPrelude

import Control.Monad (fail)
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (runLoggingT)
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL, execute_)
import Network.HTTP.Client (Manager)

import Application (makeAppMetrics)
import Auth (Authorization, generateToken)
import Database (insertToken)
import Foundation (App(..), HasDatabase, NomzServer, connectionPool, createManager, withDbConn)
import Settings (staticSettings)
import Types (UserId)

data Env = Env
  { envConnectionPool :: Pool Connection
  , envUser           :: UserId
  , envAuth           :: Authorization
  , envManager        :: Manager
  }

instance HasDatabase Env where
  connectionPool = envConnectionPool

runEnv :: Env -> (Connection -> IO a) -> IO a
runEnv env ma = either (fail . show) pure =<< runLoggingT (runReaderT (withDbConn ma) env) mempty

runServer :: Env -> NomzServer a -> IO a
runServer Env {..} ma = do
  app <- App staticSettings envConnectionPool mempty envManager
    <$> makeAppMetrics
    <*> getCurrentTime
  either (fail . show) pure =<< runLoggingT (runReaderT (runExceptT ma) app) mempty

wipeDb :: Env -> IO ()
wipeDb env = runEnv env $ \c ->
  void $ execute_ c "truncate nomz.ingredient, nomz.recipe, nomz.grocery_item restart identity"

loadEnv :: IO Env
loadEnv = do
  pool <- createPool (connectPostgreSQL "dbname=postgres host=localhost user=postgres") close 3 15 1
  (token, bcryptedToken) <- generateToken 4
  userId <- either (fail . show) pure =<< runLoggingT ( runReaderT (
    withDbConn $ \c -> do
      void $ execute_ c "truncate nomz.ingredient, nomz.recipe, nomz.grocery_item, nomz.user restart identity"
      insertToken c bcryptedToken ) pool ) mempty
  manager <- createManager
  pure $ Env pool userId token manager
