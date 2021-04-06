import ClassyPrelude

import Test.Hspec (hspec)

import qualified UnitSpec

main :: IO ()
main = hspec $ do
  UnitSpec.spec
