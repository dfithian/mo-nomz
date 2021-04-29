import ClassyPrelude
import Test.Hspec (before_, hspec)

import TestEnv (loadEnv, wipeDb)

import qualified ConversionSpec
import qualified DatabaseSpec
import qualified ParserSpec
import qualified ScrapeSpec
import qualified ServerSpec

main :: IO ()
main = do
  env <- loadEnv
  hspec $ before_ (wipeDb env) $ do
    ServerSpec.spec env
    DatabaseSpec.spec env
    ConversionSpec.spec
    ParserSpec.spec
    ScrapeSpec.spec env
