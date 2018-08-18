{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Network.Ethereum.Crypto.Address where

import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import           Data.String

import           Network.Ethereum.Data.ABI
import           Network.Ethereum.Data.Hex

import           Hath.Prelude


newtype Address = Address { fromAddress :: ByteString }
  deriving (Eq, Ord)

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

instance PutABI Address where
  putABI (Address bs) =
    let bn = bytes $ BS.replicate 12 0 <> bs :: Bytes 32
     in putABI bn

instance GetABI Address where
  getABI = Address . BS.drop 12 <$> takeN 32

nullAddress :: Address
nullAddress = "0x0000000000000000000000000000000000000000"
