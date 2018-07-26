{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Crypto
  ( module Crypto.Secp256k1
  , Address(..)
  , CompactRecSig(..)
  , pubKeyAddr
  , genSecKey
  , sha3
  ) where


import           Crypto.Hash
import           Crypto.Secp256k1

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import           Data.Monoid
import qualified Data.Text as T

import           Network.Hath.Data.Aeson
import           Network.Hath.Prelude

import           System.Entropy


newtype Address = Address { fromAddress :: ByteString }
  deriving (Eq)

instance Show Address where
  show (Address bs) = BS8.unpack (B16.encode bs)

instance Read Address where
  readsPrec p s =
    if length s == 22 && take 2 s == "0x"
       then let (a,b) = B16.decode $ fromString $ drop 2 s
             in [(Address a, BS8.unpack b)]
       else []

instance FromJSON Address where
  parseJSON (String s) =
    let r = if T.take 2 s == "0x" then T.drop 2 s else s
     in if T.length r == 40
           then Address <$> fromJsonHex (String r)
           else fail "Invalid Address"
  parseJSON _ = fail "Invalid Address"

instance ToJSON Address where
  toJSON (Address bs) = toJsonHex bs


-- Orphan instance for CompactRecSig
instance ToJSON CompactRecSig where
  toJSON (CompactRecSig r s v) = toJsonHex $
    fromShort r <> fromShort s <> (if v == 0 then "\0" else "\1")

instance FromJSON CompactRecSig where
  parseJSON s@(String _) = do
    bs <- fromJsonHex s
    let (r, rest) = BS8.splitAt 32 bs
        (s, v)    = BS8.splitAt 32 rest
        f = pure . CompactRecSig (toShort r) (toShort s)
    case v of "\0" -> f 0
              "\1" -> f 1
              _      -> fail "Sig invalid"
  parseJSON _ = fail "Sig wrong type"


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
