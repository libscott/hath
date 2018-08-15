{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Hath.Mandate where

import qualified Data.ByteString as BS
import           Data.Serialize

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC
import           Network.Ethereum.Transaction

import           Hath.Data.Aeson
import           Hath.Monad
import           Hath.Prelude


data Mandate = Mandate
  { getAddress :: Address
  , getRequiredSigs :: Int
  , getMembers :: [Address]
  , getMe :: Ident
  , getChainId :: Integer
  } deriving (Show)

loadMandate :: Has GethConfig r => Ident -> Address -> Integer -> HathE r Mandate
loadMandate me mandateAddr chainId =
  traceE "loadMandate" $ do
    ABI (r,m) <- readCall mandateAddr $ abi "getMembers()" ()
    logInfo $ "Loaded mandate at addr: " ++ show mandateAddr
    pure $ Mandate mandateAddr r m me chainId

mandateGetState :: (Has Mandate r, Has GethConfig r, GetABI a) => Bytes 32 -> HathE r a
mandateGetState key = do
  address <- asks $ getAddress . has
  traceE "mandateGetState" $ 
    unABI <$> readCall address (abi "getState(bytes32)" key)

type Nonce = Int
type Height = Int

mandateGetNonce :: (Has Mandate r, Has GethConfig r) => Bytes 32 -> HathE r (Nonce, Height)
mandateGetNonce key = do
  address <- asks $ getAddress . has
  traceE "mandateGetNonce" $
    unABI <$> readCall address (abi "getNonce(bytes32)" key)

mandateSetState :: (Has Mandate r, Has GethConfig r, PutABI a) => Bytes 32 -> a -> HathE r ()
mandateSetState key val = do
  address <- asks $ getAddress . has
  let forwardCall = abi "setState(bytes32,string)" (key, val)
  nonce <- fst <$> mandateGetNonce key
  let toSign = (address, forwardCall, key, nonce)
  Just [crs] <- campaign toSign
  let sigData = exportMultisigABI [crs]
  -- for now just test
  let proxyMethod = "proxy(address,bytes,bytes32,uint256,bytes32[],bytes32[],bytes)"
      callArgs = (address, (forwardCall, (key, (nonce, sigData))))
      callData = abi proxyMethod callArgs

  tx <- makeTransaction address callData
  out <- postTransactionSync tx
  logInfo $ "tx result: " ++ asString out

exportMultisigABI :: [CompactRecSig] -> ([Bytes 32], [Bytes 32], ByteString)
exportMultisigABI sigs =
  let f = bytes . fromShort
   in ( f . getCompactRecSigR <$> sigs
      , f . getCompactRecSigS <$> sigs
      , BS.pack $ getCompactRecSigV <$> sigs
      )

campaign :: (Has Mandate r, Has GethConfig r, PutABI a) => a -> HathE r (Maybe [CompactRecSig])
campaign a = do
  logInfo "Collecting sigs..."
  -- for now there's only one of me
  (sk,_) <- asks $ getMe . has
  let ethPrefix = "\x19\&Ethereum Signed Message:\n32"
      Just message = msg $ sha3' $ ethPrefix <> sha3' (abi "" a)
      crs = exportCompactRecSig $ signRecMsg sk message
  logInfo $ "Message hash: " ++ show message
  pure $ Just [crs]

makeTransaction :: (Has GethConfig r, Has Mandate r) => Address -> ByteString -> HathE r Transaction
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

