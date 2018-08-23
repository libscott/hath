{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Hath.Notariser.ETHKMD where

import qualified Data.ByteString as BS
import qualified Data.Serialize as Ser

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)

import           Data.Scientific
import qualified Data.Serialize as S

import           Network.Ethereum.Crypto
import           Network.Ethereum.Crypto.TrieHash
import           Network.Ethereum.Data
import           Network.Ethereum.RPC
import           Network.Bitcoin
import qualified Network.Haskoin.Internals as H
import           Network.Komodo

import           Hath.Config
import           Hath.Data.Aeson
import           Hath.Data.Binary
import           Hath.Consensus
import           Hath.Mandate
import           Hath.Mandate.Types
import           Hath.Monad
import           Hath.Prelude


-- Copy receipt roots from ETH to KMD to enable proof checking on KMD
-- TODO: Verify that requiredSigs >= notaryTxSigs


notaryTxSigs :: Int
notaryTxSigs = 11

ethKmd :: Bytes 32
ethKmd = "ETHKMD"

kmdInputAmount :: Scientific
kmdInputAmount = 0.00098

notarisationRecip :: H.ScriptOutput
notarisationRecip = H.PayPKHash $ H.getAddrHash "RXL3YXG2ceaB6C5hfJcN4fvmLH2C34knhA"


data EthNotariser = EthNotariser
  { getKomodoConfig :: BitcoinConfig
  , gethConfig :: GethConfig
  , getMandate :: Mandate
  , getCCId :: Word16
  , getNode :: ConsensusNode
  }

instance Has GethConfig EthNotariser where
  has = gethConfig

instance Has BitcoinConfig EthNotariser where
  has = getKomodoConfig

instance Has Mandate EthNotariser where
  has = getMandate

instance Has ConsensusNode EthNotariser where
  has = getNode

runEthNotariser :: Maybe Address -> HathConfig -> IO ()
runEthNotariser maddress hathConfig = do
  threadDelay 2000000
  liftIO $ initKomodo
  let gethConfig = GethConfig "http://localhost:8545"
  runHath gethConfig $ do
    conf <- loadJsonConfig $ configPath hathConfig
    mandate <- loadMandate ethKmd conf maddress
    bitcoinConf <- loadBitcoinConfig "~/.komodo/komodo.conf"
    node <- liftIO $ uncurry spawnConsensusNode $ conf .! "{ETHKMD:{seed,port}}"
    let ccId = conf .! "{ETHKMD:{ccId}}"
    let config = EthNotariser bitcoinConf gethConfig mandate ccId node
    hathReader (const config) ethNotariser


ethNotariser :: Hath EthNotariser ()
ethNotariser = do
  forever $ do
    ident@(_, myAddr) <- getBitcoinIdent
    mutxo <- chooseUtxo <$> bitcoinUtxos [myAddr]
    case mutxo of
         Nothing -> do
           logWarn "Out of UTXOs..."
           liftIO $ threadDelay $ 10 * 60 * 1000000
         Just utxo -> do
           mproposal <- getProposal
           case mproposal of
                Nothing -> do
                  liftIO $ threadDelay $ 60 * 1000000
                Just p -> runNotariserConsensus utxo p


runNotariserConsensus :: BitcoinUtxo -> (Integer, Integer) -> Hath EthNotariser ()
runNotariserConsensus utxo proposal = do
  (_, members) <- mandateGetMembers
  (_, kmdAddr) <- getBitcoinIdent
  ident <- asks $ getMe . has
  let cparams = ConsensusParams members ident 100
  r <- ask
  runConsensus cparams proposal $ do
    utxoResults <- step $ Ser2Bin (kmdAddr, getOutPoint utxo)
    let utxos = unSer2Bin . bData <$> utxoResults
    stx <- propose $ liftHath r $ Ser2Bin <$> makeTransaction utxos
    let tx = unSer2Bin stx
    ifProposer $ do
      liftHath r $ logInfo "I am proposer - not sending TX"
    liftHath r $ waitTx $ tx
  where
    liftHath :: EthNotariser -> Hath EthNotariser a -> Consensus a
    liftHath r a = liftIO $ runHath r a


waitTx :: H.Tx -> Hath EthNotariser ()
waitTx tx = do
  liftIO $ threadDelay 10000000
  error "what now?"


makeTransaction :: [(H.Address, H.OutPoint)] -> Hath r H.Tx
makeTransaction = undefined

getProposal :: Hath EthNotariser (Maybe (Integer, Integer))
getProposal = do
  mlastNota <- getLastNotarisation "ETH"
  end <- getEthProposeHeight 10
  case mlastNota of
       Nothing -> do
         logInfo $ "Starting notarisation at height: " ++ show end
         pure $ Just (end, end)
       Just (NOR{..}) -> do
         let start = fromIntegral blockNumber + 1
         case compare start end of
              EQ -> do
                logInfo "Waiting for more new blocks"
                pure Nothing
              LT -> do
                logWarn $ "Got inconsistency in heights: "
                          ++ show (start, end)
                pure Nothing
              GT -> do
                pure $ Just (start, end)


getLastNotarisation :: Has BitcoinConfig r => String -> Hath r (Maybe NotarisationData)
getLastNotarisation symbol = do
  traceE "getLastNotarisation" $ do
    val <- queryBitcoin "scanNotarisationsDB" ["0", symbol, "10000"]
    pure $ if val == Null
              then Nothing
              else do
                let bs = unHex $ val .! "{opreturn}" :: ByteString
                let Right out = S.decode bs
                 in Just out


getEthProposeHeight :: Has GethConfig r => Integer -> Hath r Integer
getEthProposeHeight n = do
  height <- eth_blockNumber
  pure $ height - mod height n


--     lastState <- mandateGetState ethKmd
--     let lastHeight = maybe 0 id $ lastState .? "{lastHeight}"
--     (_, nextHeight) <- mandateGetNonce ethKmd
--     case (lastHeight, nextHeight) of
--       (0, 0) -> do
--         -- first thing is to set a nonce to determine a height
--         logInfo "Running notarisation for the first time"
--         logInfo "Getting notarisation start block"
--         mandateIncNonce ethKmd
--       (0, n) -> do
--         logInfo $ "Got start block height: " ++ show n
--         logInfo $ "Getting next block height"
--         let newState = build "{lastHeight}" lastState n
--         mandateSetState ethKmd newState
--       (a, b) | a > b -> do
--         error "Irrecoverable error: lastHeight > nextHeight"
--       (a, b) -> do
--         logInfo $ "Got height range: " ++ show (a, b)
--         notariseToKmd (a+1, b)
--     liftIO $ threadDelay 1000000
-- 
-- notariseToKmd :: (U256, U256) -> Hath EthNotariser ()
-- notariseToKmd (from, to) = do
--   (_, myAddr) <- getBitcoinIdent
--   munspent <- chooseUtxo <$> bitcoinUtxos [myAddr]
--   case munspent of
--        Nothing -> do
--          logWarn $ "No available UTXOs... Sleeping for a bit" 
--          liftIO $ threadDelay $ 1000000 * 60 * 30
--        Just unspent -> do
--          let blockRange = [from..to]
--          blocks <- forM blockRange ethGetBlockByNumber
--          doKmdNotarisation blocks unspent


--doKmdNotarisation :: [EthBlock] -> BitcoinUtxo -> Hath EthNotariser ()
--doKmdNotarisation blocks utxo = do
--  ident@(_, myAddr) <- getBitcoinIdent
--  opRet <- getNotarisationOpReturn blocks <$> asks getCCId
--  
--  let message = toMsg opRet
--  results' <- undefined -- campaign (toMsg opRet) (Ser2Bin (myAddr, getOutPoint utxo))
--  let results = [Ballot a b (unSer2Bin c) | Ballot a b c <- results']
--  (r, _) <- mandateGetMembers
--  if length results >= r
--     then do
--       traceE "Building bitcoin tx" $ do
--         let tx = either error id $ buildNotarisationTx ident results $ H.DataCarrier opRet
--         -- logInfo $ "Sending KMD tx: " ++ show (H.txHash tx)
--         -- queryBitcoin "sendrawtransaction" [tx]
--         undefined
--     else undefined
--    
--
--
type BitcoinIdent = (H.PrvKey, H.Address)

getBitcoinIdent :: Hath EthNotariser BitcoinIdent
getBitcoinIdent = do
  (sk,_) <- asks $ getMe . getMandate
  let bitcoinKey = H.makePrvKey sk
  pure $ (bitcoinKey, H.pubKeyAddr $ H.derivePubKey bitcoinKey)
--
chooseUtxo :: [BitcoinUtxo] -> Maybe BitcoinUtxo
chooseUtxo = listToMaybe . choose
  where
    choose = reverse . sortOn (\c -> (utxoConfirmations c, utxoTxid c))
                     . filter ((==kmdInputAmount) . utxoAmount)
--
--buildNotarisationTx :: BitcoinIdent -> [Ballot (H.Address, H.OutPoint)] -> H.ScriptOutput -> Either String H.Tx
--buildNotarisationTx (pk,myAddr) ballots opRet =
--  let toSigIn (a, o) = H.SigInput (H.PayPKHash $ H.getAddrHash a) o (H.SigAll False) Nothing
--      inputs = take notaryTxSigs $ toSigIn . bData <$> sortOn bSig ballots
--
--      outputAmount = round (kmdInputAmount/100*1e8) -- TODO: calc based on inputs
--      outputs = [(notarisationRecip, outputAmount), (opRet, 0)]
--      etx = H.buildTx (H.sigDataOP <$> inputs) outputs
--      signTx tx = H.signTx tx inputs [pk]
--   in etx >>= signTx
--
--getNotarisationOpReturn :: [EthBlock] -> Word16 -> ByteString
--getNotarisationOpReturn blocks ccId =
--  let notarised = last blocks
--      mom = trieRoot $ receiptsRootTrieTrie blocks
--      opReturn =
--           BS.reverse (unHex $ blockHash notarised)
--        <> BS.reverse (enc32 $ blockNumber notarised)
--        <>            ("TESTETH\0")
--        <> BS.reverse (unSha3 mom)
--        <> BS.reverse (enc16 $ length blocks)
--        <> BS.reverse (enc16 ccId)
--   in opReturn
--  where
--    enc32 :: Integral a => a -> ByteString
--    enc32 a = Ser.encode (fromIntegral a :: Word32)
--    enc16 :: Integral a => a -> ByteString
--    enc16 a = Ser.encode (fromIntegral a :: Word16)
--    receiptsRootTrieTrie headers =
--      let (heights, roots) = (blockNumber <$> headers, blockReceiptsRoot <$> headers)
--          keys = rlpSerialize . rlpEncode . unU256 <$> heights
--       in mapToTrie $ zip keys $ unHex <$> roots
