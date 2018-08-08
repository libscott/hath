{-# LANGUAGE OverloadedStrings #-}

module Network.Hath.Bridge where

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC
import           Network.Hath.Data.Aeson
import           Network.Hath.Bridge.Utils
import           Network.Hath.Monad
import           Network.Hath.Prelude


bridge :: Hath HathConfig ()
bridge = undefined
--  -- Write block headers to multisig address on KMD
--  -- in first instance, just a single copier
--  -- later on, using multiple notaries
--  out <- runExceptT $ do
--
--    config <- loadJsonConfig "bridge"
--    myAddress <- config .@ "{myAddress}"
--
--    logInfo $ "My Address: " <> show myAddress
--    U256 balance <- queryEthereum "eth_getBalance" [toJSON myAddress, "latest"]
--    logInfo $ "Balance: " <> show balance
--
--    fail "wat"
--
--    proxyAddr <- getOrMakeBridge myAddress "createBridge(bytes16)" "KMD"
--    pure ()
--
--  case out of
--       Left s -> logError s
--       Right _ -> pure ()
--
