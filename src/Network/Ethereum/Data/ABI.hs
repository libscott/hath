{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Ethereum.Data.ABI
  ( abiMethod
  , bytesN
  , bytesLong
  ) where

import qualified Data.ByteString as BS

import           Network.Ethereum.Data.RLP
import           Network.Ethereum.Crypto
import           Network.Hath.Prelude


abiMethod :: String -> ByteString
abiMethod = BS.take 4 . sha3' . fromString

bytesPad :: ByteString -> Bool -> ByteString
bytesPad bs rev = do
  let len = BS.length bs
      padding = BS.replicate (roundToWord len) 0
   in if rev then padding <> bs
             else bs <> padding

bytesN :: ByteString -> ByteString
bytesN bs =
   if BS.length bs > 32
      then error "bytesN: data too long"
      else bytesPad bs False

bytesLong :: ByteString -> ByteString
bytesLong bs =
  let len = fromIntegral $ BS.length bs
      n = packInteger len
   in bytesPad n True <> bytesPad bs False

roundToWord :: Int -> Int
roundToWord l = (quot (l - 1) 32 + 1) * 32 - l
