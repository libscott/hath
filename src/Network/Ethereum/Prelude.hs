module Network.Ethereum.Prelude
  ( module ALL
  , exceptToFail
  ) where

import Control.Applicative as ALL
import Control.Monad as ALL (join, when, replicateM)
import Control.Monad.IO.Class as ALL (liftIO)
import Control.Monad.Trans.Except as ALL
import Control.Monad.Trans.Class as ALL

import Blockchain.Data.RLP as ALL
import Data.ByteString as ALL (ByteString)
import Data.ByteString.Lazy as ALL (toStrict)
import Data.ByteString.Short as ALL (toShort, fromShort)
import Data.Functor.Identity as ALL
import Data.List as ALL (elemIndex, sort)
import Data.Maybe as ALL (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Monoid as ALL
import Data.Set as ALL (Set)
import Data.String as ALL (fromString)
import Data.Text as ALL (Text, unpack)
import Data.Text.Encoding as ALL (encodeUtf8, decodeUtf8)
import Data.Word as ALL (Word64)

import Network.Ethereum.Errors as ALL


-- Calls fail on exception
exceptToFail :: Monad m => Except String a -> m a
exceptToFail = either fail return . runIdentity . runExceptT
