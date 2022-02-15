{-# OPTIONS_GHC -fno-warn-orphans #-}
module Serialize.Orphans where

import Prelude

import Data.CaseInsensitive (CI)
import Data.Serialize (Serialize, get, put)
import Data.Text (Text, pack, unpack)
import qualified Data.CaseInsensitive as CI

instance Serialize Text where
  put = put . unpack
  get = pack <$> get

instance (Serialize a, CI.FoldCase a) => Serialize (CI a) where
  put = put . CI.original
  get = CI.mk <$> get
