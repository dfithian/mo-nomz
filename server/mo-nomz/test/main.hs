import ClassyPrelude
import System.Environment (lookupEnv)
import Test.Hspec (before_, hspec)

import TestEnv (loadEnv, loadEnvNoDb, wipeDb)

import qualified ConversionSpec
import qualified DatabaseSpec
import qualified ParserSpec
import qualified ScrapeSpec
import qualified ServerSpec

main :: IO ()
main = do
  lookupEnv "NO_DB" >>= \case
    Just "1" -> do
      env <- loadEnvNoDb
      hspec $ do
        ConversionSpec.spec
        ParserSpec.spec
        ScrapeSpec.spec env
    _ -> do
      env <- loadEnv
      hspec $ before_ (wipeDb env) $ do
        ServerSpec.spec env
        DatabaseSpec.spec env
        ConversionSpec.spec
        ParserSpec.spec
        ScrapeSpec.spec env
