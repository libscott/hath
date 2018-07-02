{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Crypto
  ( module Crypto.Secp256k1
  , Address(..)
  , ByteString(..)
  , Signature
  , pubKeyAddr
  , genSecKey
  , sha3
  ) where


import           Crypto.Hash
import           Crypto.Secp256k1

import           Data.Aeson.Types
import qualified Data.ByteArray as BA
import           Data.ByteString as BS
import           Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import           Data.Monoid
import qualified Data.Text as T

import           Network.Ethereum.Prelude

import           System.Entropy


type Signature = CompactRecSig

newtype Address = Address { fromAddress :: ByteString }
  deriving (Eq)

instance Show Address where
  show (Address bs) = B8.unpack (B16.encode bs)

instance FromJSON Address where
  parseJSON (String s) =
    let r = if T.take 2 s == "0x" then T.drop 2 s else s
     in if T.length r == 40
           then Address <$> parseJSON (String r)
           else fail "Invalid Address"
  parseJSON _ = fail "Invalid Address"

instance ToJSON Address where
  toJSON (Address bs) = toJSON bs


sha3 :: ByteString -> ByteString
sha3 bs = BS.pack (BA.unpack (hash bs :: Digest Keccak_256))


pubKeyAddr :: PubKey -> Address
pubKeyAddr = Address . BS.drop 12 . sha3 . BS.drop 1 . exportPubKey False


genSecKey :: IO SecKey
genSecKey = do
  bytes <- getEntropy 32
  case secKey bytes of
       Just sk -> pure sk
       Nothing -> fail "IO error generating secret key"


instance ToJSON ByteString where
  toJSON = String . decodeUtf8 . B16.encode

instance FromJSON ByteString where
  parseJSON (String t) =
    case B16.decode (encodeUtf8 t) of
      (s, "") -> pure s
      _       -> fail "Invalid hex data"
  parseJSON _ = fail "Not a hex string"
