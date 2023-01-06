import NomzPrelude

import System.Environment (lookupEnv)
import Test.Hspec (hspec)

import TestEnv (loadEnv, loadEnvNoDb)

import qualified ConversionSpec
import qualified DatabaseSpec
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
      hspec $ do
        DatabaseSpec.spec env
        ConversionSpec.spec
        SiteSpec.spec env
