{-# LANGUAGE OverloadedStrings #-}

module Hath.Prelude
  ( module ALL
  , traceE
  , fromHex
  , toHex
  , expandPath
  ) where

import Control.Applicative as ALL
import Control.Exception as ALL
import Control.Monad as ALL (forM, forM_, join, when, replicateM, foldM)
import Control.Monad.IO.Class as ALL (liftIO)
import Control.Monad.Reader as ALL (ask, asks)
import Control.Monad.Trans.Class as ALL

import Data.ByteString as ALL (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Lazy as ALL (toStrict, fromStrict)
import Data.ByteString.Short as ALL (toShort, fromShort)
import Data.Function as ALL (fix)
import Data.List as ALL (elemIndex, find, findIndex, sort, sortOn)
import Data.Map as ALL (Map)
import Data.Maybe as ALL (catMaybes, fromJust, fromMaybe, mapMaybe, listToMaybe)
import Data.Monoid as ALL
import Data.Set as ALL (Set)
import Data.String as ALL (IsString, fromString)
import Data.Text as ALL (Text, unpack)
import Data.Text.Encoding as ALL (encodeUtf8, decodeUtf8)
import Data.Word as ALL (Word8, Word16, Word32, Word64)

import Network.Ethereum.Errors as ALL
import Hath.Monad as ALL
import Hath.Logging as ALL

import Lens.Micro as ALL ((<&>))

import Text.Pretty.Simple as ALL (pPrint)

import System.Directory

import Debug.Trace as ALL (traceShowId)

traceE :: String -> Hath r a -> Hath r a
traceE prefix act = do
  r <- ask
  let log e = do runHath () (logError prefix) >> throw (e::SomeException)
  liftIO $ do
    runHath r act `catch` log

fromHex :: ByteString -> ByteString
fromHex bs = let (b,r) = B16.decode bs
              in if r /= "" then error "Invalid hex" else b

toHex :: ByteString -> ByteString
toHex = B16.encode


expandPath :: FilePath -> IO FilePath
expandPath ('~':xs) = (++xs) <$> getHomeDirectory
expandPath p        = pure p
