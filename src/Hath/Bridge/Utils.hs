{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Hath.Bridge.Utils where


import           Control.Monad.Reader
import           Control.Concurrent (threadDelay)

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC
import           Network.Ethereum.Transaction
import           Hath.Data.Aeson
import           Hath.Monad
import           Hath.Prelude


data BridgeConfig = BridgeConfig
  { getChainID :: U256
  , getMyAddress :: Address
  , getSecret :: SecKey
  , hathConfig :: HathConfig
  } deriving (Show)

instance Has HathConfig BridgeConfig where
  has = hathConfig

instance Has GethConfig BridgeConfig where
  has = undefined -- TODO: fix

instance Has GethConfig HathConfig where
  has = undefined

instance FromJSON BridgeConfig where
  parseJSON = withStrictObject "BridgeConfig" $ \obj -> do
    Hex secretBytes <- obj .:- "secret"
    (sk,_,addr) <- loadSecret secretBytes
    chainId <- obj .:- "chainID"
    pure $ BridgeConfig chainId addr sk $ error "HathConfig not set"

runBridgeConfigured :: HathE BridgeConfig a -> HathE HathConfig a
runBridgeConfigured act = do
  config <- loadJsonConfig "bridge"
  hathReader (\c -> config {hathConfig=c}) act

managerAddress :: Address
managerAddress = "0x0300000000000000000000000000000000000000"


readCall :: (Has GethConfig r, FromJSON a) => Address -> ByteString -> HathE r a
readCall addr callData =
  queryEthereum "eth_call" ["{to,data}" .% (addr, Hex callData), "latest"]

makeTransaction :: Address -> ByteString -> HathE BridgeConfig Transaction
makeTransaction dest callData = do
  myAddress <- getMyAddress <$> ask
  U256 chainID <- asks getChainID
  U256 nonce <- queryEthereum "eth_getTransactionCount" [toJSON myAddress, "latest"]
  U256 gasPrice <- queryEthereum "eth_gasPrice" []
  U256 gas <- queryEthereum "eth_estimateGas" ["{to,data}" .% (dest,Hex callData)]
  let tx = Tx nonce 0 (Just dest) Nothing gasPrice (gas*2) callData chainID
  signTx tx <$> (getSecret <$> ask)

postTransactionSync :: Transaction -> HathE BridgeConfig Value
postTransactionSync tx = do
  logInfo $ "Testing transaction"
  callResult <- readCall (fromJust $ _to tx) (_data tx)
  logInfo $ "Result: " ++ asString (callResult :: Value)
  logInfo $ "Sending transaction: " ++ (asString $ toJSON tx)
  txid <- queryEthereum "eth_sendRawTransaction" [toJSON $ Hex $ encodeTx $ tx]
  logInfo $ "Send transaction, txid: " <> show txid
  fix $ \wait -> do
        liftIO $ threadDelay 1000000
        txStatus <- queryEthereum "eth_getTransactionReceipt" [txid]
        if txStatus == Null
           then wait
           else if txStatus .? "{status}" == Just (String "1")
                   then pure txStatus
                   else throwError $ "Unknown transaction status: " ++ show txStatus


getBridge :: Has BridgeConfig r => ByteString -> HathE r Address
getBridge symbol = hasReader $ do
  let callData = abi "getBridge(bytes16)" $ BytesN symbol
  RPCMaybe mexisting <- hathReader hathConfig $ readCall managerAddress callData
  maybe (throwError "Bridge does not exist; use bridge init method") pure mexisting


getOrMakeBridge :: Has BridgeConfig r => String -> ByteString -> HathE r (Address, Bool)
getOrMakeBridge createMethod symbol = hasReader $ do
  let createBridge = do
        let callData = abi createMethod $ BytesN symbol
        tx <- makeTransaction managerAddress callData
        txStatus <- postTransactionSync tx
        case txStatus .! "{contractAddress}" of
             U256 0 -> throwError "Error creating contract"
             _      -> txStatus .@ "{contractAddress}"
  let get = (,False) <$> getBridge symbol
  get `catchError` \_ -> do
         logInfo "Bridge does not exist, creating"
         (,True) <$> createBridge
