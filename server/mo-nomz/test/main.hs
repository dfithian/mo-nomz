import ClassyPrelude

import Test.Hspec (hspec)

import qualified ScrapeSpec
import qualified UnitSpec

main :: IO ()
main = hspec $ do
  UnitSpec.spec
  ScrapeSpec.spec
