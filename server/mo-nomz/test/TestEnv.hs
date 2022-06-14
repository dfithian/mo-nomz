module TestEnv where

import NomzPrelude

import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL, execute_)
import Network.HTTP.Client (Manager)

import Auth (Authorization, generateToken)
import Database (insertToken)
import Foundation (App(..), HasDatabase, NomzServer, connectionPool, createManager, withDbConn)
import Settings (testSettings)
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
  app <- App testSettings envConnectionPool mempty envManager
    <$> getCurrentTime
  either (fail . show) pure =<< runLoggingT (runReaderT (runExceptT ma) app) mempty

wipeDb :: Env -> IO ()
wipeDb env = runEnv env $ \c ->
  void $ execute_ c "truncate nomz.export, nomz.ingredient, nomz.recipe, nomz.grocery_item, nomz.recipe_cache restart identity"

loadEnv :: IO Env
loadEnv = do
  pool <- createPool (connectPostgreSQL "dbname=postgres host=localhost user=postgres") close 3 15 1
  (token, bcryptedToken) <- generateToken 4
  userId <- either (fail . show) pure =<< runLoggingT ( runReaderT (
    withDbConn $ \c -> do
      void $ execute_ c "truncate nomz.export, nomz.ingredient, nomz.recipe, nomz.grocery_item, nomz.user restart identity"
      insertToken c bcryptedToken ) pool ) mempty
  manager <- createManager
  pure $ Env pool userId token manager

loadEnvNoDb :: IO Env
loadEnvNoDb = do
  manager <- createManager
  pure $ Env (error "No connection pool") (error "No connection pool") (error "No connection pool") manager
