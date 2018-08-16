{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Hath.Notariser.ETHKMD where

import qualified Data.ByteString as BS
import qualified Data.Serialize as Ser

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)

import           Data.Scientific

import           Network.Ethereum.Crypto
import           Network.Ethereum.Crypto.TrieHash
import           Network.Ethereum.Data
import           Network.Ethereum.RPC
import           Network.Bitcoin
import qualified Network.Haskoin.Internals as H
import           Network.Komodo

import           Hath.Config
import           Hath.Data.Aeson
import           Hath.Mandate
import           Hath.Monad
import           Hath.Prelude


-- Copy receipt roots from ETH to KMD to enable proof checking on KMD


ethKmd :: Bytes 32
ethKmd = "ETHKMD"

kmdInputAmount :: Scientific
kmdInputAmount = 0.00098


data EthNotariser = EthNotariser
  { getKomodoConfig :: BitcoinConfig
  , gethConfig :: GethConfig
  , getMandate :: Mandate
  , getCCId :: Word16
  } deriving (Show)

instance Has GethConfig EthNotariser where
  has = gethConfig

instance Has BitcoinConfig EthNotariser where
  has = getKomodoConfig

instance Has Mandate EthNotariser where
  has = getMandate

runEthNotariser :: Maybe Address -> Hath EthNotariser a -> IO a
runEthNotariser maddress act = do
  liftIO $ initKomodo
  let gethConfig = GethConfig "http://localhost:8545"
  runHath gethConfig $ do

    -- load config
    conf <- loadJsonConfig "hath"

    -- load mandate
    let (Hex sk, (mandateAddr0, chainId)) = conf .! "{secret,mandates:{ETHKMD:{addr,ethChainId}}}"
    ident <- either error pure $ loadSecret sk
    let mandateAddr = maybe mandateAddr0 id maddress

    mandate <- loadMandate ident mandateAddr chainId

    bitcoinConf <- loadBitcoinConfig "~/.komodo/komodo.conf"
    let ccId = conf .! "{mandates:{ETHKMD:{ccId}}}"
    let config = EthNotariser bitcoinConf gethConfig mandate ccId
    hathReader (const config) act

ethNotariser :: Maybe Address -> IO a
ethNotariser maddress = runEthNotariser maddress $
  forever $ do
    lastState <- mandateGetState ethKmd
    let lastHeight = maybe 0 id $ lastState .? "{lastHeight}"
    (_, nextHeight) <- mandateGetNonce ethKmd
    case (lastHeight, nextHeight) of
      (0, 0) -> do
        -- first thing is to set a nonce to determine a height
        logInfo "Running notarisation for the first time"
        logInfo "Getting notarisation start block"
        mandateIncNonce ethKmd
      (0, n) -> do
        logInfo $ "Got start block height: " ++ show n
        logInfo $ "Getting next block height"
        let newState = build "{lastHeight}" lastState n
        mandateSetState ethKmd newState
      (a, b) | a > b -> do
        error "Irrecoverable error: lastHeight > nextHeight"
      (a, b) -> do
        logInfo $ "Got height range: " ++ show (a, b)
        notariseToKmd (a+1, b)
    liftIO $ threadDelay 1000000

notariseToKmd :: (U256, U256) -> Hath EthNotariser ()
notariseToKmd (from, to) = do
  (_, myAddr) <- getBitcoinIdent
  munspent <- chooseUtxo <$> bitcoinUtxos [myAddr]
  case munspent of
       Nothing -> do
         logWarn $ "No available UTXOs... Sleeping for a bit" 
         liftIO $ threadDelay $ 1000000 * 60 * 30
       Just unspent -> do
         let blockRange = [from..to]
         blocks <- forM blockRange ethGetBlockByNumber
         doKmdNotarisation blocks unspent

doKmdNotarisation :: [EthBlock] -> BitcoinUtxo -> Hath EthNotariser ()
doKmdNotarisation blocks utxo = do
  ident <- getBitcoinIdent
  traceE "Building bitcoin tx" $ do
    opRet <- getNotarisationOpReturn blocks <$> asks getCCId
    let tx = either error id $ getNotarisationTx ident utxo blocks opRet
    logInfo $ "Sending KMD tx: " ++ show (H.txHash tx)
    -- queryBitcoin "sendrawtransaction" [tx]

type BitcoinIdent = (H.PrvKey, H.Address)

getBitcoinIdent :: Hath EthNotariser BitcoinIdent
getBitcoinIdent = do
  (sk,_) <- asks $ getMe . getMandate
  let bitcoinKey = H.makePrvKey sk
  pure $ (bitcoinKey, H.pubKeyAddr $ H.derivePubKey bitcoinKey)

chooseUtxo :: [BitcoinUtxo] -> Maybe BitcoinUtxo
chooseUtxo = listToMaybe . choose
  where
    choose = reverse . sortOn (\c -> (utxoConfirmations c, utxoTxid c))
                     . filter ((==kmdInputAmount) . utxoAmount)

notarisationOutput0 = H.PayPKHash $ H.getAddrHash "RXL3YXG2ceaB6C5hfJcN4fvmLH2C34knhA"

getNotarisationTx :: BitcoinIdent -> BitcoinUtxo -> [EthBlock] -> H.ScriptOutput
                  -> Either String H.Tx
getNotarisationTx (pk,myAddr) utxo blocks opRet = 
  let op = getOutPoint utxo
      os = H.PayPKHash $ H.getAddrHash myAddr
      sigIn = H.SigInput os op (H.SigAll False) Nothing
      outputAmount = round (kmdInputAmount/100*1e8)
      etx = H.buildTx [H.sigDataOP sigIn] [(notarisationOutput0, outputAmount), (opRet, 0)]
      signTx tx = H.signTx tx [sigIn] [pk]
   in etx >>= signTx

getNotarisationOpReturn :: [EthBlock] -> Word16 -> H.ScriptOutput
getNotarisationOpReturn blocks ccId =
  let notarised = last blocks
      mom = trieRoot $ receiptsRootTrieTrie blocks
      opReturn =
           BS.reverse (unHex $ blockHash notarised)
        <> BS.reverse (enc32 $ blockNumber notarised)
        <>            ("TESTETH\0")
        <> BS.reverse (unSha3 mom)
        <> BS.reverse (enc16 $ length blocks)
        <> BS.reverse (enc16 ccId)
   in H.DataCarrier opReturn
  where
    enc32 :: Integral a => a -> ByteString
    enc32 a = Ser.encode (fromIntegral a :: Word32)
    enc16 :: Integral a => a -> ByteString
    enc16 a = Ser.encode (fromIntegral a :: Word16)
    receiptsRootTrieTrie headers =
      let (heights, roots) = (blockNumber <$> headers, blockReceiptsRoot <$> headers)
          keys = rlpSerialize . rlpEncode . unU256 <$> heights
       in mapToTrie $ zip keys $ unHex <$> roots
