{-# LANGUAGE OverloadedStrings #-}


-- | Code gratefully borrowed from https://github.com/blockapps/ethereum-rlp
--
-- | Copyright 2016 BlockApps, Inc
-- | 
-- | Licensed under the Apache License, Version 2.0 (the "License");
-- | you may not use this file except in compliance with the License.
-- | You may obtain a copy of the License at
-- | 
-- |     http://www.apache.org/licenses/LICENSE-2.0
-- | 
-- | Unless required by applicable law or agreed to in writing, software
-- | distributed under the License is distributed on an "AS IS" BASIS,
-- | WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- | See the License for the specific language governing permissions and
-- | limitations under the License.

-- | The RLP module provides a framework within which serializers can be built, described in the Ethereum Yellowpaper (<http://gavwood.com/paper.pdf>).
--
-- The 'RLPObject' is an intermediate data container, whose serialization rules are well defined.  By creating code that converts from a
-- given type to an 'RLPObject', full serialization will be specified.  The 'RLPSerializable' class provides functions to do this conversion.

module Network.Ethereum.Data.RLP (
  RLPObject(..),
  formatRLPObject,
  RLPSerializable(..),
  rlpSplit,
  rlpSerialize,
  rlpDeserialize,
  rlpNull,
  packInteger,
  unpackInteger
  ) where

import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Internal
import           Data.Word
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import           Numeric


-- | An internal representation of generic data, with no type information.
--
-- End users will not need to directly create objects of this type (an 'RLPObject' can be created using 'rlpEncode'),
-- however the designer of a new type will need to create conversion code by making their type an instance 
-- of the RLPSerializable class. 
data RLPObject =
  RLPScalar Word8 |
  RLPString B.ByteString |
  RLPArray [RLPObject]
  deriving (Eq, Ord)

instance Show RLPObject where
  show (RLPScalar w) = show w
  show (RLPString b) = show b
  show (RLPArray a) = show a

-- | Converts objects to and from 'RLPObject's.
class RLPSerializable a where
  rlpDecode::RLPObject->a
  rlpEncode::a->RLPObject

instance Pretty RLPObject where
  pretty (RLPArray objects) =
    encloseSep (text "[") (text "]") (text ", ") $ pretty <$> objects
  pretty (RLPScalar n) = text $ "0x" ++ showHex n ""
  pretty (RLPString s) = text $ "0x" ++ BC.unpack (B16.encode s)

formatRLPObject::RLPObject->String
formatRLPObject = show . pretty

splitAtWithError::Int->B.ByteString->(B.ByteString, B.ByteString)
splitAtWithError i s | i > B.length s = error "splitAtWithError called with n > length arr"
splitAtWithError i s = B.splitAt i s

getLength::Int->B.ByteString->(Integer, B.ByteString)
getLength sizeOfLength bytes =
  (unpackInteger $ B.take sizeOfLength bytes, B.drop sizeOfLength bytes)

rlpSplit::B.ByteString->(RLPObject, B.ByteString)
rlpSplit input =
  case B.head input of
    x | x >= 192 && x <= 192+55 ->
      let (arrayData, nextRest) =
            splitAtWithError (fromIntegral x - 192) $ B.tail input
      in (RLPArray $ getRLPObjects arrayData, nextRest)

    x | x >= 0xF8 && x <= 0xFF ->
      let 
        (arrLength, restAfterLen) = getLength (fromIntegral x - 0xF7) $ B.tail input
        (arrayData, nextRest) = splitAtWithError (fromIntegral arrLength) restAfterLen
      in (RLPArray $ getRLPObjects arrayData, nextRest)

    x | x >= 128 && x <= 128+55 ->
      let
        (strList, nextRest) = splitAtWithError (fromIntegral $ x - 128) $ B.tail input
      in 
       (RLPString strList, nextRest)

    x | x >= 0xB8 && x <= 0xBF ->
      let 
        (strLength, restAfterLen) = getLength (fromIntegral x - 0xB7) $ B.tail input
        (strList, nextRest) = splitAtWithError (fromIntegral strLength) restAfterLen
      in
       (RLPString strList, nextRest)

    x | x < 128 -> (RLPScalar x, B.tail input)
            
    x -> error ("Missing case in rlpSplit: " ++ show x)

    
getRLPObjects :: ByteString -> [RLPObject]
getRLPObjects "" = []
getRLPObjects theData =
  let (obj, rest) = rlpSplit theData
   in obj : getRLPObjects rest


rlp2Bytes::RLPObject->[Word8]
rlp2Bytes (RLPScalar val) = [fromIntegral val]
rlp2Bytes (RLPString s) | B.length s <= 55 = 0x80 + fromIntegral (B.length s):B.unpack s
rlp2Bytes (RLPString s) =
  [0xB7 + fromIntegral (length lengthAsBytes)] ++ lengthAsBytes ++ B.unpack s
  where
    lengthAsBytes = integerToBytes $ B.length s
rlp2Bytes (RLPArray innerObjects) =
  if length innerBytes <= 55
  then 0xC0 + fromIntegral (length innerBytes):innerBytes
  else let lenBytes = integerToBytes $ length innerBytes
       in [0xF7 + fromIntegral (length lenBytes)] ++ lenBytes ++ innerBytes
  where
    innerBytes = concat $ rlp2Bytes <$> innerObjects

--TODO- Probably should just use Data.Binary's 'Binary' class for this

-- | Converts bytes to 'RLPObject's.
--
-- Full deserialization of an object can be obtained using @rlpDecode . rlpDeserialize@.
rlpDeserialize::B.ByteString->RLPObject
rlpDeserialize s = 
  case rlpSplit s of
    (o, x) | B.null x -> o
    _ -> error ("parse error converting ByteString to an RLP Object: " ++ show (B.unpack s))


-- | Converts 'RLPObject's to bytes.
--
-- Full serialization of an object can be obtained using @rlpSerialize . rlpEncode@.
rlpSerialize::RLPObject->B.ByteString
rlpSerialize o = B.pack $ rlp2Bytes o


rlpNull :: RLPObject
rlpNull = RLPString ""


instance RLPSerializable Integer where
  rlpEncode 0             = RLPString B.empty
  rlpEncode x | x < 0     = error "cannot encode negative numbers in RLP"
  rlpEncode x | x < 128   = RLPScalar $ fromIntegral x
  rlpEncode x             = RLPString $ packInteger x
  rlpDecode (RLPScalar x) = fromIntegral x
  rlpDecode (RLPString s) = unpackInteger s
  rlpDecode (RLPArray _)  = error "rlpDecode called for Integer for array"

instance RLPSerializable RLPObject where
  rlpEncode = id
  rlpDecode = id

instance RLPSerializable B.ByteString where
    rlpEncode x | B.length x == 1 && B.head x < 128 = RLPScalar $ B.head x
    rlpEncode s = RLPString s
      
    rlpDecode (RLPScalar x) = B.singleton x
    rlpDecode (RLPString s) = s
    rlpDecode x = error ("rlpDecode for ByteString not defined for: " ++ show x)


-- serialization for tuples, triples, etc. of serializable types
instance (RLPSerializable a, RLPSerializable b) => RLPSerializable (a,b) where
  rlpEncode (a,b) = RLPArray [rlpEncode a, rlpEncode b]
  rlpDecode (RLPArray [a,b]) = (rlpDecode a, rlpDecode b)
  rlpDecode x = error $ "rlpDecode for tuples not defined for " ++ show x

instance (RLPSerializable a, RLPSerializable b, RLPSerializable c) => RLPSerializable (a,b,c) where
  rlpEncode (a,b,c) = RLPArray [rlpEncode a, rlpEncode b, rlpEncode c]
  rlpDecode (RLPArray [a,b,c]) = (rlpDecode a, rlpDecode b, rlpDecode c)
  rlpDecode x = error $ "rlpDecode for triples not defined for " ++ show x

instance 
  ( RLPSerializable a 
  , RLPSerializable b
  , RLPSerializable c
  , RLPSerializable d 
  ) => RLPSerializable (a,b,c,d) where
  rlpEncode (a,b,c,d) = RLPArray [rlpEncode a, rlpEncode b, rlpEncode c, rlpEncode d]
  rlpDecode (RLPArray [a,b,c,d]) = (rlpDecode a, rlpDecode b, rlpDecode c, rlpDecode d)
  rlpDecode x = error $ "rlpDecode for 4-tuples not defined for " ++ show x

instance 
  ( RLPSerializable a
  , RLPSerializable b 
  , RLPSerializable c
  , RLPSerializable d
  , RLPSerializable e
  ) => RLPSerializable (a,b,c,d,e) where
  rlpEncode (a,b,c,d,e) = RLPArray [rlpEncode a, rlpEncode b, rlpEncode c, rlpEncode d, rlpEncode e]
  rlpDecode (RLPArray [a,b,c,d,e]) = (rlpDecode a, rlpDecode b, rlpDecode c, rlpDecode d, rlpDecode e)
  rlpDecode x = error $ "rlpDecode for 5-tuples not defined for " ++ show x

instance 
  ( RLPSerializable a
  , RLPSerializable b 
  , RLPSerializable c
  , RLPSerializable d
  , RLPSerializable e
  , RLPSerializable f
  ) => RLPSerializable (a,b,c,d,e,f) where
  rlpEncode (a,b,c,d,e,f) = RLPArray [rlpEncode a, rlpEncode b, rlpEncode c, rlpEncode d, rlpEncode e, rlpEncode f]
  rlpDecode (RLPArray [a,b,c,d,e,f]) = (rlpDecode a, rlpDecode b, rlpDecode c, rlpDecode d, rlpDecode e, rlpDecode f)
  rlpDecode x = error $ "rlpDecode for 6-tuples not defined for " ++ show x

instance RLPSerializable a => RLPSerializable [a] where
  rlpEncode xs = RLPArray $ rlpEncode <$> xs
  rlpDecode (RLPArray xs) = rlpDecode <$> xs
  rlpDecode rlp = error "rlpDecode [a]"

packInteger :: Integer -> B.ByteString
packInteger = B.pack . integerToBytes


integerToBytes :: (Bits a, Integral a) => a -> [Word8]
integerToBytes = reverse . pack
  where
    pack 0 = []
    pack x = fromIntegral (x .&. 255) : pack (x `shiftR` 8)


unpackInteger :: B.ByteString -> Integer
unpackInteger = unpack . B.unpack
  where
    unpack [] = 0
    unpack (byte:rest) = fromIntegral byte `shift` (8 * length rest) + unpack rest
