import NomzPrelude

import System.Environment (lookupEnv)
import Test.Hspec (before_, hspec)

import TestEnv (loadEnv, loadEnvNoDb, wipeDb)

import qualified ConversionSpec
import qualified DatabaseSpec
import qualified ServerSpec
import qualified SiteSpec

main :: IO ()
main = do
  lookupEnv "NO_DB" >>= \case
    Just "1" -> do
      env <- loadEnvNoDb
      hspec $ do
        ConversionSpec.spec
        SiteSpec.spec env
    _ -> do
      env <- loadEnv
      hspec $ before_ (wipeDb env) $ do
        ServerSpec.cacheSpec env
        DatabaseSpec.spec env
        ConversionSpec.spec
        SiteSpec.spec env
