{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Hath.Mandate where

import qualified Data.ByteString as BS
import           Data.Serialize
import           Data.Typeable

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC
import           Network.Ethereum.Transaction

import           Hath.Data.Aeson
import           Hath.Monad
import           Hath.Prelude
import           Hath.Mandate.Agree


data Mandate = Mandate
  { getAddress :: Address
  , getMe :: Ident
  , getChainId :: Integer
  , getProc :: AgreementProcess
  }

instance Has AgreementProcess Mandate where
  has = getProc

mandateGetMembers :: (Has Mandate r, Has GethConfig r) => Hath r (Int, [Address])
mandateGetMembers = do
  addr <- asks $ getAddress . has
  unABI <$> readCall addr (abi "getMembers()" ())

loadMandate :: Has GethConfig r => Ident -> Address -> Integer -> Hath r Mandate
loadMandate me mandateAddr chainId =
  traceE "loadMandate" $ do
    proc <- liftIO $ spawnAgree
    logInfo $ "Loaded mandate at addr: " ++ show mandateAddr
    pure $ Mandate mandateAddr me chainId proc

mandateGetState :: (Has Mandate r, Has GethConfig r, GetABI a) => Bytes 32 -> Hath r a
mandateGetState key = do
  address <- asks $ getAddress . has
  traceE "mandateGetState" $ 
    unABI <$> readCall address (abi "getState(bytes32)" key)

type Nonce = Int
type Height = U256

mandateGetNonce :: (Has Mandate r, Has GethConfig r) => Bytes 32 -> Hath r (Nonce, Height)
mandateGetNonce key = do
  address <- asks $ getAddress . has
  traceE "mandateGetNonce" $
    unABI <$> readCall address (abi "getNonce(bytes32)" key)

mandateIncNonce :: (Has Mandate r, Has GethConfig r) => Bytes 32 -> Hath r ()
mandateIncNonce key = mandateProxy key nullAddress "" >> pure ()

data AgreeFail = AgreeFail deriving (Show)
instance Exception AgreeFail

mandateProxy :: (Has Mandate r, Has GethConfig r) => Bytes 32 -> Address -> ByteString ->
                Hath r Value
mandateProxy key target forwardCall = do
  address <- asks $ getAddress . has
  nonce <- fst <$> mandateGetNonce key

  let toSign = ethMsg (target, forwardCall, key, nonce)
  results <- campaign toSign ()
  requiredSigs <- fst <$> mandateGetMembers
  when (length results < requiredSigs) (throw AgreeFail)

  let sigData = exportMultisigABI $ bSig <$> results
      proxyMethod = "proxy(address,bytes,bytes32,uint256,bytes32[],bytes32[],bytes)"
      callArgs = (target, (forwardCall, (key, (nonce, sigData))))
      callData = abi proxyMethod callArgs

  tx <- makeTransaction address callData
  postTransactionSync tx

ethMsg :: PutABI a => a -> Msg
ethMsg a = toMsg $ "\x19\&Ethereum Signed Message:\n32" <> abi "" a

mandateSetState :: (Has Mandate r, Has GethConfig r, PutABI a) => Bytes 32 -> a -> Hath r ()
mandateSetState key val = do
  address <- asks $ getAddress . has
  let forwardCall = abi "setState(bytes32,string)" (key, val)
  out <- mandateProxy key address forwardCall
  logInfo $ "setState result: " ++ asString out

exportMultisigABI :: [CompactRecSig] -> ([Bytes 32], [Bytes 32], ByteString)
exportMultisigABI sigs =
  let f = bytes . fromShort
   in ( f . getCompactRecSigR <$> sigs
      , f . getCompactRecSigS <$> sigs
      , BS.pack $ getCompactRecSigV <$> sigs
      )

-- Collect signatures and payloads from members
campaign :: (Has Mandate r, Has GethConfig r, Serialize a, Typeable a) => Msg -> a -> Hath r [Ballot a]
campaign message myData = do
  logInfo "Collecting sigs..."
  (sk,myAddr) <- asks $ getMe . has
  (_, m) <- mandateGetMembers
  let crs = exportCompactRecSig $ signRecMsg sk message
  hathReader (getProc . has) $ agreeCollectSigs message (Ballot myAddr crs myData) m

makeTransaction :: (Has GethConfig r, Has Mandate r) => Address -> ByteString -> Hath r Transaction
makeTransaction dest callData = do
  (sk,myAddress) <- asks $ getMe . has
  chainID <- asks $ getChainId . has
  U256 nonce <- queryEthereum "eth_getTransactionCount" [toJSON myAddress, "latest"]
  --U256 gasPrice <- queryEthereum "eth_gasPrice" []
  let gasPrice = 1
  ---U256 gas <- queryEthereum "eth_estimateGas" ["{to,data}" .% (dest,Hex callData)]
  let gas = 1000000
  let tx = Tx nonce 0 (Just dest) Nothing gasPrice (gas*2) callData chainID
  pure $ signTx tx sk
