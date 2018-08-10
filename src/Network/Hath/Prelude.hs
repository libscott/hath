{-# LANGUAGE OverloadedStrings #-}

module Network.Hath.Prelude
  ( module ALL
  , liftEither
  , traceE
  , fromHex
  , toHex
  , expandPath
  ) where

import Control.Applicative as ALL
import Control.Monad as ALL (forM, forM_, join, when, replicateM)
import Control.Monad.IO.Class as ALL (liftIO)
import Control.Monad.Reader as ALL (ask, asks)
import Control.Monad.Except as ALL
import Control.Monad.Trans.Class as ALL

import Data.ByteString as ALL (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Lazy as ALL (toStrict)
import Data.ByteString.Short as ALL (toShort, fromShort)
import Data.List as ALL (elemIndex, sort, sortOn)
import Data.Maybe as ALL (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Monoid as ALL
import Data.Set as ALL (Set)
import Data.String as ALL (IsString, fromString)
import Data.Text as ALL (Text, unpack)
import Data.Text.Encoding as ALL (encodeUtf8, decodeUtf8)
import Data.Word as ALL (Word8, Word64)

import Network.Ethereum.Errors as ALL
import Network.Hath.Monad.Types as ALL
import Network.Hath.Logging as ALL

import Text.Pretty.Simple as ALL (pPrint)

import System.Directory

import Debug.Trace as ALL (traceShowId)

-- mtl 2.2.2 will provide this
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

traceE :: String -> HathE r a -> HathE r a
traceE prefix act =
  catchError act $ \e ->
    throwError (prefix ++ "\n" ++ e)

fromHex :: ByteString -> ByteString
fromHex bs = let (b,r) = B16.decode bs
              in if r /= "" then error "Invalid hex" else b

toHex :: ByteString -> ByteString
toHex = B16.encode


expandPath :: FilePath -> IO FilePath
expandPath ('~':xs) = (++xs) <$> getHomeDirectory
expandPath p        = pure p
