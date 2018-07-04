{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Prelude
  ( module ALL
  , exceptToFail
  , fromHex
  , toHex
  ) where

import Control.Applicative as ALL
import Control.Monad as ALL (forM, forM_, join, when, replicateM)
import Control.Monad.IO.Class as ALL (liftIO)
import Control.Monad.Trans.Except as ALL
import Control.Monad.Trans.Class as ALL

import Data.ByteString as ALL (ByteString)
import qualified Data.ByteString.Base16 as B16
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
import Data.Word as ALL (Word8, Word64)

import Network.Ethereum.Errors as ALL

import Debug.Trace as ALL (traceShowId)

-- Calls fail on exception
exceptToFail :: Monad m => Except String a -> m a
exceptToFail = either fail return . runIdentity . runExceptT


fromHex :: ByteString -> ByteString
fromHex bs = let (b,r) = B16.decode bs
              in if r /= "" then error "Invalid hex" else b

toHex :: ByteString -> ByteString
toHex = B16.encode
