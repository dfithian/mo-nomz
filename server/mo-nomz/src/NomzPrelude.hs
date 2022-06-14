module NomzPrelude
  ( module Prelude
  , module Control.Applicative
  , module Control.Arrow
  , module Control.Exception
  , module Control.Monad
  , module Control.Monad.Catch
  , module Control.Monad.Except
  , module Control.Monad.IO.Class
  , module Control.Monad.Logger
  , module Control.Monad.Reader
  , module Control.Monad.Trans.Reader
  , module Data.ByteString
  , module Data.CaseInsensitive
  , module Data.Default
  , module Data.Function
  , module Data.HashMap.Strict
  , module Data.List
  , module Data.Map.Strict
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Pool
  , module Data.Proxy
  , module Data.Set
  , module Data.Text
  , module Data.Time.Clock
  , module Data.Traversable
  , module GHC.Generics
  , nubOrd, uncurry3, uncurry4, tshow, headMay, lastMay
  ) where

import Control.Applicative ((<|>), optional)
import Control.Arrow (first, left, right, second)
import Control.Exception (Exception, SomeException, fromException)
import Control.Monad (replicateM, unless, void, when)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.Except (ExceptT, MonadError, mapExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, MonadLoggerIO, askLoggerIO, runLoggingT)
import Control.Monad.Reader (MonadReader, ask, asks, local)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Containers.ListUtils (nubOrd)
import Data.Default (def)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.List (find, groupBy, intercalate, sortBy, sortOn)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Monoid (Sum(..))
import Data.Pool (Pool, createPool, withResource)
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Prelude
import qualified Data.Text as Text

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (x, y, z, w) = f x y z w

tshow :: Show a => a -> Text
tshow = Text.pack . show

headMay :: [a] -> Maybe a
headMay = \case
  x:_ -> Just x
  [] -> Nothing

lastMay :: [a] -> Maybe a
lastMay = headMay . reverse
