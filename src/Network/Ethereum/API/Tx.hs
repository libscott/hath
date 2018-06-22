{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.API.Tx where

import           Data.Aeson hiding (encode)
import           Data.Aeson.Types (Parser, parseEither)
import           Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import           Data.Text.Encoding
import           Blockchain.Data.RLP

import           Network.Ethereum.API.Utils
import           Network.Ethereum.Crypto
import           Network.Ethereum.Prelude
import           Network.Ethereum.Transaction


encodeTxHex = B16.encode . rlpSerialize . rlpEncode


encodeTx :: JsonMethod
encodeTx = pureMethod $ \tx ->
  pure $ do
    let encode = String . decodeUtf8 . encodeTxHex
    pure $ encode (tx :: Transaction)


--signTxAct :: ByteString -> ExceptT Err IO Value
--signTxAct keyHex = do
--  let (kb,ke) = B16.decode keyHex
--      sk = secKey kb
--  txHex <- lift B8.getContents


-- decodeTx :: JsonMethod
-- decodeTx = pureMethod $ \obj -> do
--   tx <- obj .: "hex"
--   pure $ toJSON <$> TX.decodeTx tx
