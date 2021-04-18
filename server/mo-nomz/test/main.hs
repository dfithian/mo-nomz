import ClassyPrelude

import Test.Hspec (hspec)

import qualified ConversionSpec
import qualified ScrapeSpec

main :: IO ()
main = hspec $ do
  ConversionSpec.spec
  ScrapeSpec.spec
