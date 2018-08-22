{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module Network.Ethereum.Crypto.Hash
  ( Sha3(..)
  , sha3
  , sha3'
  ) where

import           Crypto.Hash

import           Data.Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

import           Network.Ethereum.Data.ABI
import           Network.Ethereum.Data.Hex
import           Network.Ethereum.Data.RLP
import           Hath.Prelude


newtype Sha3 = Sha3 { unSha3 :: ByteString }
  deriving (Eq, Ord, RLPSerializable)

instance Show Sha3 where
  show (Sha3 bs) = asString (toHex bs)

instance Read Sha3 where
  readsPrec a s
    | take 2 s == "0x" = readsPrec a $ drop 2 s
    | length s == 64   = [(Sha3 $ fromHex $ fromString s, "")]
    | otherwise        = []

instance IsString Sha3 where
  fromString s = read s

instance FromJSON Sha3 where
  parseJSON val = do
    Hex bs <- parseJSON val
    if BS.length bs == 32
       then pure $ Sha3 bs
       else fail "malformed hash"

instance ToJSON Sha3 where
  toJSON = toJSON . Hex . unSha3

instance PutABI Sha3 where
  putABI (Sha3 bs) = putABI (bytes bs :: Bytes 32)

sha3' :: ByteString -> ByteString
sha3' bs = BS.pack (BA.unpack (hash bs :: Digest Keccak_256))

sha3 :: ByteString -> Sha3
sha3 = Sha3 . sha3'
