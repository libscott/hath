{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Ethereum.Crypto
  ( module ALL
  , Address(..)
  , CompactRecSig(..)
  , Ident
  , pubKeyAddr
  , genSecKey
  , loadSecret
  , nullAddress
  ) where


import           Control.Monad.Fail (MonadFail)

import           Crypto.Hash
import           Crypto.Secp256k1 as ALL

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import           Data.Monoid
import qualified Data.Text as T

import           Network.Ethereum.Crypto.Hash as ALL
import           Network.Ethereum.Crypto.TrieHash as ALL
import           Network.Ethereum.Data.Hex
import           Network.Ethereum.Data.RLP
import           Hath.Data.Aeson
import           Hath.Prelude

import           System.Entropy


newtype Address = Address { fromAddress :: ByteString }
  deriving (Eq)

instance Show Address where
  show (Address bs) = "0x" <> BS8.unpack (B16.encode bs)

instance Read Address where
  readsPrec p s =
    if length s == 42 && take 2 s == "0x"
       then let (a,b) = B16.decode $ fromString $ drop 2 s
             in [(Address a, BS8.unpack b)]
       else []

instance IsString Address where
  fromString = read

instance FromJSON Address where
  parseJSON val = do
    Hex bs <- parseJSON val
    if BS.length bs == 20 then pure $ Address bs
                          else fail "Invalid Address"

instance ToJSON Address where
  toJSON (Address bs) = toJSON $ Hex bs

nullAddress :: Address
nullAddress = "0x0000000000000000000000000000000000000000"

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

pubKeyAddr :: PubKey -> Address
pubKeyAddr = Address . BS.drop 12 . sha3' . BS.drop 1 . exportPubKey False

type Ident = (SecKey, Address)

loadSecret :: ByteString -> Either String Ident
loadSecret secretBS = do
  sk <- maybe (Left "Invalid SK bytes?") Right $ secKey secretBS
  pure (sk, pubKeyAddr $ derivePubKey sk)

genSecKey :: IO SecKey
genSecKey = do
  bytes <- getEntropy 32
  case secKey bytes of
       Just sk -> pure sk
       Nothing -> fail "IO error generating secret key"
