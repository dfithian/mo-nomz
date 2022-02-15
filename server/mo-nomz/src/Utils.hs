module Utils where

import Prelude

import Data.Text (Text)
import qualified Data.Text as Text

tshow :: Show a => a -> Text
tshow = Text.pack . show

headMay :: [a] -> Maybe a
headMay = \case
  x:_ -> Just x
  [] -> Nothing

lastMay :: [a] -> Maybe a
lastMay = headMay . reverse
