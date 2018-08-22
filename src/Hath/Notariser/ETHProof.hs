
module Hath.Notariser.ETHProof where

import Network.Ethereum.Crypto
import Network.Ethereum.RPC

import Hath.Data.Aeson
import Hath.Config
import Hath.Prelude


-- tx -> receiptsroot
-- receiptsroot -> MoM (scan on KMD for notarisations from ETH)
-- MoM -> MoMoM (scan on KMD for notarisations to XXX)
proveEthKmdTransaction :: HathConfig -> Sha3 -> IO ()
proveEthKmdTransaction conf txid = do
  runHath conf $ do
    out <- eth_getTransactionReceipt txid
    logInfo $ show (out::Value)

