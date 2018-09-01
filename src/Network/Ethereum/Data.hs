{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Data
  ( module ALL
  ) where

import           Network.Ethereum.Data.ABI as ALL
import           Hath.Data.Hex as ALL
import           Network.Ethereum.Data.RLP as ALL


instance RLPSerializable Hex where
  rlpEncode = rlpEncode . unHex
  rlpDecode = Hex . rlpDecode

instance RLPSerializable U256 where
  rlpEncode = rlpEncode . unU256
  rlpDecode = U256 . rlpDecode
