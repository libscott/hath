{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Data.TrieHash where

import qualified Data.ByteString as BS

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data.RLP

import           Network.Hath.Prelude

type Nibbles = [Word8]
type HexMap = [(Nibbles, ByteString)]

patriciaRoot :: [ByteString] -> ByteString
patriciaRoot bss =
  let hexMap = orderedHexMap bss
   in sha3 $ rlpSerialize $ hash256rlp hexMap

hash256rlp :: HexMap -> RLPObject
hash256rlp []      = rlpNull
hash256rlp [(k,v)] = RLPArray [rlpEncode $ hexPrefixEncode k True, rlpEncode v]
hash256rlp xa      =
  let sharedPre = longestSharedPrefix $ fst <$> xa
      doPrefix =
        let prefix = rlpEncode $ hexPrefixEncode sharedPre False
         in RLPArray [prefix, hex256aux $ trimMap (length sharedPre) xa]
   in if null sharedPre then do16Node xa else doPrefix

do16Node :: HexMap -> RLPObject
do16Node xa@((k,v):xs) =
   RLPArray $ if 0 == length k
                 then do16NodeItems xs 0 ++ [rlpEncode v]
                 else do16NodeItems xa 0 ++ [rlpNull]

do16NodeItems :: HexMap -> Word8 -> [RLPObject]
do16NodeItems _     16 = []
do16NodeItems items i  =
  let part = takeWhile (\(k,_) -> take 1 k == [i]) items
      r = if null part then rlpNull else hex256aux (trimMap 1 part)
   in r : do16NodeItems (drop (length part) items) (i+1)

hex256aux :: HexMap -> RLPObject
hex256aux xs =
  let out = hash256rlp xs
      ser = rlpSerialize out
   in if BS.length ser < 32
         then out
         else rlpEncode $ sha3 ser

hexPrefixEncode :: Nibbles -> Bool -> ByteString
hexPrefixEncode s isLeaf =
  let rem = mod (length s) 2
      rest = fromNibbles $ drop rem s
      fst = (fromIntegral rem + if isLeaf then 2 else 0) * 16
             + if rem == 1 then s!!0 else 0
   in BS.cons fst rest

trimMap :: Int -> HexMap -> HexMap
trimMap i xs = [(drop i x, bs) | (x, bs) <- xs]

toNibbles :: ByteString -> Nibbles
toNibbles bs = BS.unpack bs >>=
  \b -> let (d,q) = divMod b 16 in [d,q]

fromNibbles :: Nibbles -> ByteString
fromNibbles [] = ""
fromNibbles (a:b:xs) = BS.cons (a*16+b) (fromNibbles xs)

longestSharedPrefix :: [Nibbles] -> Nibbles
longestSharedPrefix [] = []
longestSharedPrefix (x:xs) = foldr f x xs
  where f a b = fst <$> takeWhile (uncurry (==)) (zip a b)

orderedHexMap :: [ByteString] -> HexMap
orderedHexMap bss =
  let packKey = toNibbles . rlpSerialize . rlpEncode
      items = zip (packKey <$> [0::Integer ..]) bss
   in sortOn fst items
