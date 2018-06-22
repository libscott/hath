{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Transaction.Types where


import           Data.Aeson.Types
import           Data.ByteString as BS

import           Blockchain.Data.RLP
import           Network.Ethereum.Data.Aeson
import           Network.Ethereum.Crypto
import           Network.Ethereum.Prelude


data From = Nobody | From Address | Signed Signature
  deriving (Eq, Show)


instance ToJSON From where
  toJSON Nobody = Null
  toJSON (From a) = toJSON a
  toJSON (Signed (CompactRecSig r s v)) =
         toJSON [ toJsonHex $ fromShort r
                , toJsonHex $ fromShort s
                , toJSON v
                ]


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
    let toRlpFrom (Signed (CompactRecSig r s v)) =
              [ rlpEncode (27 + fromIntegral v :: Integer)
              , rlpEncode $ fromShort r
              , rlpEncode $ fromShort s
              ]
        toRlpFrom _ = []

        items = [ rlpEncode $ _nonce tx
                , rlpEncode $ _gasPrice tx
                , rlpEncode $ _gas tx
                , rlpEncode $ maybe "" fromAddress $ _to tx
                , rlpEncode $ _value tx
                , rlpEncode $ _data tx
                ] ++ toRlpFrom (_from tx)

     in RLPArray items

  rlpDecode (RLPArray [n,gp,g,to,val,d]) = 
    let sto = rlpDecode to
        mto = case BS.length sto of
                   20 -> Just (Address sto)
                   0  -> Nothing
                   _  -> error "Invalid address"
     in Tx (rlpDecode n) (rlpDecode val) mto Nobody (rlpDecode gp) (rlpDecode g) (rlpDecode d) 0

  rlpDecode (RLPArray [n,gp,g,to,val,d,v,r,s]) =
    let tx = rlpDecode $ RLPArray [n,gp,g,to,val,d]
        v' = rlpDecode v :: Integer
        [r',s'] = toShort . rlpDecode <$> [r,s]
        from = if [rlpDecode r, rlpDecode s] == [0,0::Integer]
                       then Nobody
                       else Signed crs
        crs = CompactRecSig r' s' $ fromIntegral v'
     in tx { _from = from }

  rlpDecode _ = error "Invalid RLP Transaction"


instance ToJSON Transaction where
  toJSON tx =
    object [ "nonce"    .= _nonce tx
           , "value"    .= _value tx
           , "to"       .= _to tx
           , "from"     .= _from tx
           , "gasPrice" .= _gasPrice tx
           , "gas"      .= _gas tx
           , "data"     .= (toJsonHex $ _data tx)
           , "chainId"  .= _chainId tx
           ]


instance FromJSON Transaction where
  parseJSON = withStrictObject "Transaction" $ \o -> do
    Tx <$> o .:- "nonce"
       <*> o .:- "value"
       <*> o .:- "to"
       <*> o .:- "from"
       <*> o .:- "gasPrice"
       <*> o .:- "gas"
       <*> (o .:- "data" >>= fromJsonHex)
       <*> o .:- "chainId"
