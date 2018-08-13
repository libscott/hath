{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Hath.Bridge.KMD where

import           Control.Concurrent (threadDelay)

import           Network.Bitcoin
import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC
import           Hath.Data.Aeson
import           Hath.Prelude
import           Hath.Monad
import           Hath.Bridge.Utils
import qualified Network.Haskoin.Internals as H
import           Network.Komodo


data KomodoBridge = KomodoBridge
  { getBridgeConfig :: BridgeConfig
  , getBitcoinConfig :: BitcoinConfig
  } deriving (Show)

instance Has BitcoinConfig KomodoBridge where
  has = getBitcoinConfig

instance Has BridgeConfig KomodoBridge where
  has = getBridgeConfig

instance Has HathConfig KomodoBridge where
  has = has . getBridgeConfig

instance Has GethConfig KomodoBridge where
  has = error "No GethConfig"

runKMDBridgeConfigured :: Has BridgeConfig r => HathE KomodoBridge a -> HathE r a
runKMDBridgeConfigured act = do
    logInfo $ "Loading KMD RPC config"
    config <- loadBitcoinConfig "~/.komodo/komodo.conf"
    hathReader (\c -> KomodoBridge (has c) config) act


-- 1. Load config
-- 2. Check or create bridge
initKMDBridge :: H.TxHash -> Hath HathConfig ()
initKMDBridge openingBalanceHash = do
  liftIO $ initKomodo

  out <- runExceptT $ runBridgeConfigured $ runKMDBridgeConfigured $ do

    myAddress <- getMyAddress . has <$> ask
    logInfo $ "My Address: " <> show myAddress
    U256 balance <- queryEthereum "eth_getBalance" [toJSON myAddress, "latest"]
    logInfo $ "Balance: " <> show balance

    (bridgeAddr, created) <- getOrMakeBridge "createBridgeKMD(bytes16)" "KMD"
    logInfo $ "Bridge addr: " ++ show bridgeAddr

    kmdAddress <- getBitcoinAddress
    logInfo $ "Bitcoin addr: " ++ show kmdAddress

    lastState <- bridgeLastState bridgeAddr
    let doInit = do
          pendingHash <- spendOpeningBalance openingBalanceHash
          let newState = build "{pending}" lastState pendingHash
          hasReader $ do
            tx <- makeTransaction bridgeAddr $
                  abi "updateState(bytes)" $ toStrict $ encode newState
            postTransactionSync tx
          pure ()

    case lastState .? "{pending}" :: Maybe Value of
         Nothing -> doInit <* logInfo "Bridge initialized"
         Just txid -> do
           logInfo $ "Bridge already initialized: " ++ asString lastState
  
  case out of
       Left s -> logError s
       Right _ -> pure ()


runKMDBridge :: Hath HathConfig ()
runKMDBridge = do
  liftIO $ initKomodo

  out <- runExceptT $ runBridgeConfigured $ runKMDBridgeConfigured $ do

    myAddress <- getMyAddress . has <$> ask
    logInfo $ "My Address: " <> show myAddress
    U256 balance <- queryEthereum "eth_getBalance" [toJSON myAddress, "latest"]
    logInfo $ "Balance: " <> show balance

    bridgeAddr <- getBridge "KMD"
    logInfo $ "Bridge addr: " ++ show bridgeAddr

    -- First thing: Make a call to the bridge to get the last transaction
    lastState <- bridgeLastState bridgeAddr
    case lastState .? "{pending}" of
         Nothing -> fail "No state; run initKMDBridge"
         Just hash -> bridgeResume lastState hash

  case out of
       Left s -> logError s
       Right _ -> pure ()


bridgeFirstRun :: Value -> HathE KomodoBridge ()
bridgeFirstRun lastState = do
  -- See if we have a balance to play with
  bAddress <- getBitcoinAddress
  r <- queryBitcoin "getaddressbalance" [toJSON bAddress]
  logInfo $ show bAddress ++ asString (r::Value)

bridgeResume :: Value -> H.TxHash -> HathE KomodoBridge ()
bridgeResume = error "bridgeresume"

spendOpeningBalance :: H.TxHash -> HathE KomodoBridge H.TxHash
spendOpeningBalance hash = do
  logInfo "Spending opening balance"
  (sigIn,balance) <- getUtxo
  let feeSat = 20000
  H.PubKeyAddress addr160 <- getBitcoinAddress
  spendTx <- liftEither $ H.buildTx [H.sigDataOP sigIn] [(H.PayPKHash addr160, balance-feeSat)] 
  sk <- getBitcoinSecKey
  signed <- liftEither $ H.signTx spendTx [sigIn] [sk]
  traceE ("Sign and send: " ++ show (H.txHash signed)) $ do
    let send = queryBitcoin "sendrawtransaction" [toJSON signed, toJSON False]
               <* logInfo "Spent opening balance"
        getExists = (queryBitcoin "getrawtransaction" [toJSON $ H.txHash signed, Number 1]
                     >>= (.@"{txid}")) <* logInfoN "Tx already in blockchain"
    getExists `catchError` \_ -> send
  where
    getUtxo = traceE "getting opening balance tx" $ do
      btcAddress <- getBitcoinAddress
      res <- queryBitcoin "getrawtransaction" [toJSON hash, Number 1]
      tx <- res .@ "{hex}"
      let spendable (H.TxOut amount scriptBS:xs) i =
            case H.decodeOutputBS scriptBS of
                 Right os@(H.PayPKHash h160) | H.PubKeyAddress h160 == btcAddress ->
                      let op = H.OutPoint hash i
                          sigIn = H.SigInput os op (H.SigAll False) Nothing
                       in Just (sigIn,amount)
                 _ -> if null xs then Nothing else spendable xs (i+1)
      maybe (throwError "Opening balance not spendable") pure $ spendable (H.txOut tx) 0

bridgeLastState :: Address -> HathE KomodoBridge Value
bridgeLastState bridgeAddr = do
  Hex bs  <- readCall bridgeAddr $ abi "lastState()" ()
  traceE "Decoding lastState" $ liftEither $ eitherDecodeStrict' bs


getBitcoinAddress :: Has BridgeConfig r => HathE r H.Address
getBitcoinAddress = do
  H.pubKeyAddr . H.derivePubKey <$> getBitcoinSecKey

getBitcoinSecKey :: Has BridgeConfig r => HathE r H.PrvKey
getBitcoinSecKey = asks $ H.makePrvKey . getSecret . has

-- runHathConfigured $ initKMDBridge "d5089554b8ceded72e3f034eb5e6cd7d381d3d2ae582b3981b8f8e93b958658f"
