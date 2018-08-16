{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Ethereum.RPC
  ( GethConfig(..)
  , EthBlock(..)
  , queryEthereum
  , readCall
  , postTransactionSync
  , ethGetBlockByNumber
  ) where

import           Control.Concurrent (threadDelay)

import           Data.Function (fix)

import           Network.Ethereum.Data
import           Network.Ethereum.Crypto
import           Network.Ethereum.Transaction

import           Network.JsonRpc

import           Hath.Data.Aeson
import           Hath.Monad
import           Hath.Prelude


newtype GethConfig = GethConfig { gethEndpoint :: String }
  deriving (Show)

queryEthereum :: (Has GethConfig r, ToJSON b, FromJSON a) => Text -> b -> Hath r a
queryEthereum method params = do
  endpoint <- asks $ gethEndpoint . has
  queryJsonRpc endpoint method params

readCall :: (Has GethConfig r, FromJSON a) => Address -> ByteString -> Hath r a
readCall addr callData =
  queryEthereum "eth_call" ["{to,data}" .% (addr, Hex callData), "latest"]

postTransactionSync :: Has GethConfig r => Transaction -> Hath r Value                      
postTransactionSync tx = do                                                         
  logInfo $ "Testing transaction"                                                   
  callResult <- readCall (fromJust $ _to tx) (_data tx)                             
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


ethGetBlockByNumber :: (Integral a, ToJSON a, Has GethConfig r) => a -> Hath r EthBlock
ethGetBlockByNumber number = queryEthereum "eth_getBlockByNumber" (number, False)

data EthBlock = EthBlock
  { blockNumber :: U256
  , blockHash :: Hex
  , blockReceiptsRoot :: Hex
  } deriving (Show)

instance FromJSON EthBlock where
  parseJSON val = do
    obj <- parseJSON val
    EthBlock <$> obj .: "number"
             <*> obj .: "hash"
             <*> obj .: "receiptsRoot"
