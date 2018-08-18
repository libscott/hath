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
  , sign
  , recover
  , recoverAddr
  , toMsg
  ) where


import           Control.Monad.Fail (MonadFail)

import           Crypto.Hash
import           Crypto.Secp256k1 as ALL hiding (recover)
import qualified Crypto.Secp256k1 as Secp256k1

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import           Data.Binary
import           Data.Monoid
import qualified Data.Text as T

import           Network.Ethereum.Crypto.Address as ALL
import           Network.Ethereum.Crypto.Hash as ALL
import           Network.Ethereum.Crypto.TrieHash as ALL
import           Network.Ethereum.Data.ABI
import           Network.Ethereum.Data.Hex
import           Network.Ethereum.Data.RLP
import           Hath.Data.Aeson
import           Hath.Prelude

import           System.Entropy


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

instance Binary CompactRecSig where
  put (CompactRecSig r s v) = put r >> put s >> put v
  get = CompactRecSig <$> get <*> get <*> get

pubKeyAddr :: PubKey -> Address
pubKeyAddr = Address . BS.drop 12 . sha3' . BS.drop 1 . exportPubKey False

recoverAddr :: Msg -> CompactRecSig -> Maybe Address
recoverAddr msg crs = do
  rs <- importCompactRecSig crs
  pubKeyAddr <$> recover rs msg

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

recover :: RecSig -> Msg -> Maybe PubKey
recover rs message =
  let s = convertRecSig rs
      (_, bad) = normalizeSig s
   in if bad then Nothing
             else Secp256k1.recover rs message

toMsg :: ByteString -> Msg
toMsg = fromJust . msg . sha3'

sign :: SecKey -> Msg -> CompactRecSig
sign sk = exportCompactRecSig . signRecMsg sk
