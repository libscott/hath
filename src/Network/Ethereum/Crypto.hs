{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Crypto
  ( module Crypto.Secp256k1
  , Address(..)
  , Signature
  , pubkeyToAddress
  , genSecKey
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
  show (Address bs) = "0x" <> B8.unpack (B16.encode bs)

instance FromJSON Address where
  parseJSON (String s) =
    let (p,r) = T.splitAt 2 s
        (d,t) = B16.decode $ encodeUtf8 r
     in if p == "0x" && T.length r == 40 && t == ""
           then pure (Address d)
           else fail "Invalid Address"
  parseJSON _ = fail "Invalid Address"

instance ToJSON Address where
  toJSON (Address a) = toJSON $ show a



pubkeyToAddress :: PubKey -> Address
pubkeyToAddress pk =
  let bs = exportPubKey True pk
      h = (hash bs :: Digest Keccak_256)
   in Address $ BS.pack $ Prelude.drop 12 $ BA.unpack h


genSecKey :: IO SecKey
genSecKey = do
  bytes <- getEntropy 32
  case secKey bytes of
       Just sk -> pure sk
       Nothing -> fail "IO error generating secret key"

