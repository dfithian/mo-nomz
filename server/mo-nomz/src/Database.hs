module Database where

import NomzPrelude

import Database.PostgreSQL.Simple
  ( Only(Only), Connection, execute, fromOnly, query, query_, returning
  )

import Auth (BcryptedAuthorization)
import Postgres.Orphans ()
import Types (UserId)

health :: Connection -> IO ()
health conn = do
  [Only (1 :: Int)] <- query_ conn "select 1"
  pure ()

insertToken :: Connection -> BcryptedAuthorization -> IO UserId
insertToken conn token = do
  [(Only userId)] <- returning conn "insert into nomz.user (token, is_valid) values (?, ?) returning id" [(token, True)]
  pure userId

fetchToken :: Connection -> UserId -> IO (Maybe BcryptedAuthorization)
fetchToken conn userId = do
  token <- query conn "select token from nomz.user where id = ? and is_valid" (Only userId) >>= \case
    [(Only token)] -> pure token
    _ -> pure Nothing
  void $ execute conn "update nomz.user set last_active = now() where id = ?" (Only userId)
  pure token

updateUserPing :: Connection -> UserId -> Text -> Maybe Text -> IO ()
updateUserPing conn userId version targetMay = do
  void $ execute conn "update nomz.user set version = ?, target = ? where id = ?" (version, targetMay, userId)

selectRecentUsers :: Connection -> IO (Int, Int, Int, Int)
selectRecentUsers conn = do
  let q interval = maybe 0 fromOnly . headMay <$> query_ conn ("select count(id) from nomz.user where last_active >= now () - interval '" <> interval <> "' and coalesce(target, 'device') <> 'simulator'")
  (,,,)
    <$> q "1 day"
    <*> q "7 day"
    <*> q "28 day"
    <*> q "365 day"
