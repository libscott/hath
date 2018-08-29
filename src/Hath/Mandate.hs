{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}

module Hath.Mandate where

import           Control.Distributed.Process.Serializable (Serializable)

import qualified Data.ByteString as BS
import           Data.Serialize
import           Data.Typeable

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC
import           Network.Ethereum.Transaction

import           Hath.Config
import           Hath.Data.Aeson
import           Hath.Monad
import           Hath.Prelude


mandateGetData :: (GetABI a, Has GethConfig r) => Address -> Hath r (a, [Address], U256)
mandateGetData addr = do
  unABI <$> readCall addr (abi "getMandate()" ())



--data Mandate = Mandate
--  { getAddress :: Address
--  , getMe :: Ident
--  , getChainId :: Integer
--  , getAppKey :: Bytes 32
--  }
--
--


--
--loadMandate :: Has HathConfig r => Bytes 32 -> Maybe Address -> Hath r Mandate
--loadMandate (Bytes appKey) maddr =
--  traceE "loadMandate" $ do
--    allConf <- asks $ configVal . has
--
--    let s = fromString $ asString $ "{secret," <> appKey <> "}"
--        (Hex sk, conf) = allConf .! s
--
--    pure $ Mandate (maybe (conf .! "{addr}") id maddr)
--                   (either error id $ loadSecret sk)
--                   (conf .! "{ethChainId}")
--                   (Bytes appKey)


--mandateGetState :: (Has Mandate r, Has GethConfig r, GetABI a) => Bytes 32 -> Hath r a
--mandateGetState key = do
--  address <- asks $ getAddress . has
--  traceE "mandateGetState" $ 
--    unABI <$> readCall address (abi "getState(bytes32)" key)
--
--type Nonce = Int
--type Height = U256
--
--mandateGetNonce :: (Has Mandate r, Has GethConfig r) => Bytes 32 -> Hath r (Nonce, Height)
--mandateGetNonce key = do
--  address <- asks $ getAddress . has
--  traceE "mandateGetNonce" $
--    unABI <$> readCall address (abi "getNonce(bytes32)" key)
--
--mandateIncNonce :: (Has Mandate r, Has GethConfig r) => Bytes 32 -> Hath r ()
--mandateIncNonce key = do
--  logInfo "mandateIncNonce"
--  mandateProxy key nullAddress "" >> pure ()

--data AgreeFail = AgreeFail deriving (Show)
--instance Exception AgreeFail

--mandateProxy :: (Has Mandate r, Has GethConfig r) => Bytes 32 -> Address -> ByteString ->
--                Hath r Value
--mandateProxy key target forwardCall = do
--  address <- asks $ getAddress . has
--  nonce <- fst <$> mandateGetNonce key
--
--  let toSign = ethMsg (target, forwardCall, key, nonce)
--  
--  (sigs, sender) <- agreeMsgFast toSign
--
--  let sigData = exportMultisigABI $ sigs
--      proxyMethod = "proxy(address,bytes,bytes32,uint256,bytes32[],bytes32[],bytes)"
--      callArgs = (target, (forwardCall, (key, (nonce, sigData))))
--      callData = abi proxyMethod callArgs
--
--  myAddress <- asks $ snd . getMe . has
--  when (myAddress == sender) $ do
--    logInfo "I am the sender!"
--    tx <- makeTransaction address callData
--    --postTransactionSync tx
--    logInfo $ "Not posting transaction to mandateProxy!"
--
--  error "EOF"

--ethMsg :: PutABI a => a -> Msg
--ethMsg a = toMsg $ "\x19\&Ethereum Signed Message:\n32" <> sha3' (abi "" a)
--
--mandateSetState :: (Has Mandate r, Has GethConfig r, PutABI a) => Bytes 32 -> a -> Hath r ()
--mandateSetState key val = do
--  logInfo "mandateSetState"
--  address <- asks $ getAddress . has
--  let forwardCall = abi "setState(bytes32,string)" (key, val)
--  out <- mandateProxy key address forwardCall
--  logInfo $ "setState result: " ++ asString out
--
--exportMultisigABI :: [CompactRecSig] -> ([Bytes 32], [Bytes 32], ByteString)
--exportMultisigABI sigs =
--  let f = bytes . fromShort
--   in ( f . getCompactRecSigR <$> sigs
--      , f . getCompactRecSigS <$> sigs
--      , BS.pack $ getCompactRecSigV <$> sigs
--      )
--
--makeTransaction :: (Has GethConfig r, Has Mandate r) => Address -> ByteString -> Hath r Transaction
--makeTransaction dest callData = do
--  (sk,myAddress) <- asks $ getMe . has
--  chainID <- asks $ getChainId . has
--  U256 nonce <- queryEthereum "eth_getTransactionCount" [toJSON myAddress, "latest"]
--  logInfo $ "Transaction nonce: " ++ show nonce
--  --U256 gasPrice <- queryEthereum "eth_gasPrice" []
--  let gasPrice = 1
--  ---U256 gas <- queryEthereum "eth_estimateGas" ["{to,data}" .% (dest,Hex callData)]
--  let gas = 1000000
--  let tx = Tx nonce 0 (Just dest) Nothing gasPrice (gas*2) callData chainID
--  pure $ signTx tx sk
