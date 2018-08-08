{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Contracts where

import qualified Data.ByteString as BS
import           Data.Serialize

import           Network.Ethereum.Data (packInteger)

import           Network.Haskoin.Script
import           Network.Haskoin.Crypto

import           Network.Hath.Prelude


bitcoinMultisig :: Word8 -> [PubKey] -> Script
bitcoinMultisig n pks = do
  let n' = BS.singleton n
      pks' = opPushData . encode <$> pks
      m = packInteger $ fromIntegral $ length pks

      out = [opPushData n'] ++ pks' ++ [opPushData m, OP_CHECKMULTISIG]

  if n == 0 || fromIntegral n > length pks || length pks == 0
     then error $ "Invalid n of m: " ++ show (n, length pks)
     else Script out
