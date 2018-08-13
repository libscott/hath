{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hath.Notariser.ETHKMD where

import Network.Ethereum.Crypto
import Network.Ethereum.Data
import Network.Ethereum.RPC
import Network.Bitcoin

import Hath.Bridge.Utils
import Hath.Data.Aeson
import Hath.Monad
import Hath.Prelude


-- Copy MoMs from ETH to KMD


data EthNotariser = EthNotariser
  { getKomodoConfig :: BitcoinConfig
  , getEthSockPath :: FilePath
  , getMandateAddress :: Address
  } deriving (Show)

instance Has GethConfig EthNotariser where
  has n = GethConfig $ getEthSockPath n

runEthNotariser :: HathE EthNotariser a -> IO (Either String a)
runEthNotariser act =
  runHath () $ runExceptT $ do
    bitcoinConf <- loadBitcoinConfig "~/.komodo/komodo.conf"
    let mandate = "0x8288295979fdbd828606bdd72425bb9d9fea7e49"
    let config = EthNotariser bitcoinConf "/home/scott/code/wing/kmdcontracts/testChainData/geth.ipc"
                              mandate
    hathReader (const config) act

ethNotariser :: IO (Either String ())
ethNotariser = runEthNotariser $ do
  let callData = abi "getMembers()" ()

  mandate <- asks getMandateAddress
  ABI (U256 requiredSigs, addresses) <- readCall mandate callData
  logInfo $ "Parties: " ++ show requiredSigs ++ " of " ++ show (addresses :: [Address])
  
  ABI lastState <- readCall mandate $ abi "getState(bytes32)" $ BytesN "ETHKMD"
  if lastState == Null
     then ethNotariserFirstTime
     else ethNotariserResume lastState

ethNotariserFirstTime :: HathE EthNotariser ()
ethNotariserFirstTime = do
  U256 currentHeight <- queryEthereum "eth_blockNumber" []
  logInfo $ "Starting notarisation for the frist time from ETH block: " ++ show currentHeight

ethNotariserResume :: Value -> HathE EthNotariser ()
ethNotariserResume = undefined
