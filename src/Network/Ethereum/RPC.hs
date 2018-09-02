{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Ethereum.RPC where

import           Control.Concurrent (threadDelay)

import           Network.Ethereum.Data
import           Network.Ethereum.Crypto
import           Network.Ethereum.Transaction as Tx
import           Network.Ethereum.Types

import           Network.JsonRpc

import           Hath.Config
import           Hath.Data.Aeson
import           Hath.Monad
import           Hath.Prelude


queryEthereum :: (Has GethConfig r, ToJSON b, FromJSON a) => Text -> b -> Hath r a
queryEthereum method params = do
  uri <- asks $ gethEndpoint . has
  let endpoint = HttpEndpoint $ fromString uri
  queryJsonRpc endpoint method params

readCall :: (Has GethConfig r, FromJSON a) => Address -> ByteString -> Hath r a
readCall addr callData =
  queryEthereum "eth_call" ["{to,data}" .% (addr, Hex callData), "latest"]

postTransactionSync :: Has GethConfig r => Transaction -> Hath r Value
postTransactionSync tx = do
  logInfo $ "Testing transaction"
  callResult <- readCall (fromJust $ _to tx) (Tx._data tx)
  logInfo $ "Result: " ++ asString (callResult :: Value)
  logInfo $ "Sending transaction: " ++ (show $ txid tx)
  txid <- queryEthereum "eth_sendRawTransaction" [toJSON $ Hex $ encodeTx $ tx]
  logInfo $ "Send transaction, txid: " <> show txid
  fix $ \wait -> do
        liftIO $ threadDelay 1000000
        txStatus <- queryEthereum "eth_getTransactionReceipt" [txid::Value]
        if txStatus == Null
           then wait
           else if txStatus .? "{status}" == Just (U256 1)
                   then pure txStatus
                   else error $ "Unknown transaction status: " ++ show txStatus

data RPCMaybe a = RPCMaybe (Maybe a)
  deriving (Show)

instance FromJSON a => FromJSON (RPCMaybe a) where
  parseJSON (String "0x") = pure $ RPCMaybe Nothing
  parseJSON val = RPCMaybe . Just <$> parseJSON val



data EthBlock = EthBlock
  { ethBlockNumber :: U256
  , ethBlockHash :: Sha3
  , ethBlockReceiptsRoot :: Sha3
  , ethBlockTransactions :: [Sha3]
  } deriving (Show)

instance FromJSON EthBlock where
  parseJSON val = do
    obj <- parseJSON val
    EthBlock <$> obj .: "number"
             <*> obj .: "hash"
             <*> obj .: "receiptsRoot"
             <*> obj .: "transactions"


eth_getTransactionReceipt :: Has GethConfig r => Sha3 -> Hath r TransactionReceipt
eth_getTransactionReceipt h = queryEthereum "eth_getTransactionReceipt" [h]

eth_blockNumber :: Has GethConfig r => Hath r U256
eth_blockNumber = queryEthereum "eth_blockNumber" ()

eth_getBlockByHash :: Has GethConfig r => Sha3 -> Hath r EthBlock
eth_getBlockByHash n = queryEthereum "eth_getBlockByHash" (n, False)

eth_getBlockByNumber :: Has GethConfig r => U256 -> Hath r EthBlock
eth_getBlockByNumber n = queryEthereum "eth_getBlockByNumber" (n, False)
