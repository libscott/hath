{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Transaction.Types where


import           Data.Aeson.Types
import           Data.ByteString
import qualified Data.ByteString.Base16 as B16

import           Blockchain.Data.RLP
import           Network.Ethereum.Crypto
import           Network.Ethereum.Prelude


data From = Nobody | From Address | Signed Signature
  deriving (Eq, Show)

instance ToJSON From where
  toJSON Nobody = Null
  toJSON (From a) = toJSON a
  toJSON (Signed s) = String "dis nigga finna signed"


instance FromJSON From where
  parseJSON Null = pure Nobody
  parseJSON (String s) = From <$> parseJSON (String s)
  parseJSON _ = fail "Invalid From"


data Transaction = Tx
  { _nonce    :: Integer
  , _value    :: Integer
  , _to       :: Maybe Address
  , _from     :: From
  , _gasPrice :: Integer
  , _gas      :: Integer
  , _data     :: ByteString
  , _chainId  :: Integer
  } deriving (Eq, Show)


instance RLPSerializable Transaction where
  rlpEncode tx =
    RLPArray [ rlpEncode $ _nonce tx
             , rlpEncode $ _gasPrice tx
             , rlpEncode $ _gas tx
             , rlpEncode $ maybe "" fromAddress $ _to tx
             , rlpEncode $ _value tx
             , rlpEncode $ _data tx
             , rlpEncode $ _chainId tx
             , rlpEncode $ (0 :: Integer)
             , rlpEncode $ (0 :: Integer)
             ]
  rlpDecode _ = undefined


instance ToJSON Transaction where
  toJSON tx =
    object [ "nonce"    .= _nonce tx
           , "value"    .= _value tx
           , "to"       .= _to tx
           , "from"     .= _from tx
           , "gasPrice" .= _gasPrice tx
           , "gas"      .= _gas tx
           , "data"     .= (decodeUtf8 $ B16.encode $ _data tx)
           , "chainId"  .= _chainId tx
           ]


instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \o -> do
    Tx <$> o .: "nonce"
       <*> o .: "value"
       <*> o .: "to"
       <*> o .: "from"
       <*> o .: "gasPrice"
       <*> o .: "gas"
       <*> (fst . B16.decode . encodeUtf8 <$> o .: "data")
       <*> o .: "chainId"
