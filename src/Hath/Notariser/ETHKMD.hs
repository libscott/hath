{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hath.Notariser.ETHKMD where

import Control.Concurrent (threadDelay)
import Network.Ethereum.Crypto
import Network.Ethereum.Data
import Network.Ethereum.RPC
import Network.Bitcoin

import Hath.Data.Aeson
import Hath.Mandate
import Hath.Monad
import Hath.Prelude


-- Copy MoMs from ETH to KMD


data EthNotariser = EthNotariser
  { getKomodoConfig :: BitcoinConfig
  , gethConfig :: GethConfig
  , getMandate :: Mandate
  } deriving (Show)

instance Has GethConfig EthNotariser where
  has = gethConfig

instance Has Mandate EthNotariser where
  has = getMandate

runEthNotariser :: Maybe Address -> HathE EthNotariser a -> IO (Either String a)
runEthNotariser maddress act = do
  let gethConfig = GethConfig "http://localhost:8545"
  runHath gethConfig $ runExceptT $ do

    -- load config
    conf <- loadJsonConfig "hath"

    -- load mandate
    (Hex sk, (mandateAddr0, chainId)) <- conf .@ "{secret,mandates:{ETHKMD:{addr,chainId}}}"
    ident <- liftEither $ loadSecret sk
    let mandateAddr = maybe mandateAddr0 id maddress

    mandate <- loadMandate ident mandateAddr chainId

    bitcoinConf <- loadBitcoinConfig "~/.komodo/komodo.conf"
    let config = EthNotariser bitcoinConf gethConfig mandate
    hathReader (const config) act

ethNotariser :: Maybe Address -> IO (Either String ())
ethNotariser maddress = runEthNotariser maddress $
  forever $ do
    lastState <- mandateGetState "ETHKMD"

    if lastState == Null
       then do
         logInfo $ "Starting notarisation for the first time"
         agreeHeight lastState
       else error "what now"

    liftIO $ threadDelay 1000000


agreeHeight :: Value -> HathE EthNotariser ()
agreeHeight lastState = do
  -- At this point we should create a tx to etheruem to register agreement
  -- to notarise a block. So we do a state change to update the data.

  -- get new state
  let newState = build "{op}" lastState ("notarise"::Text)
  logInfo $ "Last state: " ++ asString lastState
  logInfo $ "Next state: " ++ asString newState

  mandateSetState "ETHKMD" newState
  

