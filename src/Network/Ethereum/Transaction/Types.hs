{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Transaction.Types where

import qualified Data.ByteString.Char8 as BS8

import           Network.Hath.Data.Aeson
import           Network.Ethereum.Data.RLP
import           Network.Ethereum.Crypto
import           Network.Hath.Prelude


data Transaction = Tx
  { _nonce    :: Integer
  , _value    :: Integer
  , _to       :: Maybe Address
  , _sig      :: Maybe CompactRecSig
  , _gasPrice :: Integer
  , _gas      :: Integer
  , _data     :: ByteString
  , _chainId  :: Integer
  } deriving (Eq, Show)


instance RLPSerializable Transaction where
  rlpEncode tx =
    let body =
          [ rlpEncode $ _nonce tx
          , rlpEncode $ _gasPrice tx
          , rlpEncode $ _gas tx
          , rlpEncode $ maybe "" fromAddress $ _to tx
          , rlpEncode $ _value tx
          , rlpEncode $ _data tx
          ]

        encodeSig (CompactRecSig r s v) =
          [ rlpEncode $ encodeSpecialV v $ _chainId tx
          , rlpEncode $ unpackInteger $ fromShort r
          , rlpEncode $ unpackInteger $ fromShort s
          ]

        noSig = [rlpEncode $ _chainId tx, RLPString "", RLPString ""]

   in RLPArray $ body ++ maybe noSig encodeSig (_sig tx)

  rlpDecode rlp@(RLPArray [_,_,_,_,_,_]) =
    let (n,gp,g,to,val,d) = rlpDecode rlp
        mto = case BS8.length to of
                   20 -> Just (Address to)
                   0  -> Nothing
                   _  -> error "Invalid address"
     in Tx n val mto Nothing gp g d 0

  rlpDecode (RLPArray [n,gp,g,to,val,d,_v,_r,_s]) =
    let tx = rlpDecode $ RLPArray [n,gp,g,to,val,d]

        pad32 "" = ""
        pad32 bs = if BS8.length bs < 32 then pad32 ("\0" <> bs) else bs
        [r,s] = toShort . pad32 . rlpDecode <$> [_r,_s]

        sv = rlpDecode _v
        (v, c) = decodeSpecialV sv
        crs = CompactRecSig r s v

     in if r == "" && s == ""
           then tx { _sig = Just crs, _chainId = c }
           else tx { _sig = Nothing, _chainId = sv }

  rlpDecode _ = error "Invalid RLP Transaction"


encodeSpecialV :: Word8 -> Integer -> Integer
encodeSpecialV v chainId = fromIntegral v + chainId * 2 + 35

decodeSpecialV :: Integer -> (Word8, Integer)
decodeSpecialV v' = (fromIntegral (mod (v'+1) 2), quot (v' - 35) 2)


instance ToJSON Transaction where
  toJSON tx =
    object [ "nonce"    .= _nonce tx
           , "value"    .= _value tx
           , "to"       .= _to tx
           , "sig"      .= _sig tx
           , "gasPrice" .= _gasPrice tx
           , "gas"      .= _gas tx
           , "data"     .= toJsonHex (_data tx)
           , "chainId"  .= _chainId tx
           ]


instance FromJSON Transaction where
  parseJSON = withStrictObject "Transaction" $ \o -> do
    Tx <$> o .:-  "nonce"
       <*> o .:-  "value"
       <*> o .:-? "to"
       <*> o .:-? "sig"
       <*> o .:-  "gasPrice"
       <*> o .:-  "gas"
       <*> (o .:-? "data" >>= fromJsonHex . maybe "" id)
       <*> o .:-  "chainId"
