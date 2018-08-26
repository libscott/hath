
module Hath.Notariser.ETHKMD where

import qualified Data.Serialize as Ser

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception.Safe (catchAny)
import           Control.Monad (forever)

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import           Data.Scientific
import           Data.Typeable

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC
import           Network.Bitcoin
import qualified Network.Haskoin.Internals as H
import           Network.Komodo

import           Hath.Config
import           Hath.Data.Aeson
import           Hath.Data.Binary
import           Hath.Consensus
import           Hath.Notariser.UTXOs
import           Hath.Mandate
import           Hath.Monad
import           Hath.Prelude
import Debug.Trace


notaryTxSigs :: Int
notaryTxSigs = 11

ethKmd :: Bytes 32
ethKmd = "ETHKMD"

kmdInputAmount :: Word64
kmdInputAmount = 9800

kmdCCid :: Word16
kmdCCid = 25555

notarisationRecip :: H.ScriptOutput
notarisationRecip = H.PayPKHash $ H.getAddrHash "RXL3YXG2ceaB6C5hfJcN4fvmLH2C34knhA"

consensusTimeout :: Int
consensusTimeout = 10 * 1000000


data EthNotariser = EthNotariser
  { getKomodoConfig :: BitcoinConfig
  , getNode :: ConsensusNode
  , gethConfig :: GethConfig
  , getMandate :: Address
  , getSecret :: SecKey
  }

instance Has GethConfig    EthNotariser where has = gethConfig
instance Has BitcoinConfig EthNotariser where has = getKomodoConfig
instance Has ConsensusNode EthNotariser where has = getNode


-- Entry point for ETH notariser program
--
runEthNotariser :: GethConfig -> ConsensusNetworkConfig -> Address -> H.Address -> IO ()
runEthNotariser gethConfig consensusConfig mandateAddr kmdAddr = do
  threadDelay 2000000
  initKomodo
  runHath () $ do
    bitcoinConf <- loadBitcoinConfig "~/.komodo/komodo.conf"

    -- resolve pk
    wif <- hathReader (const bitcoinConf) $ queryBitcoin "dumpprivkey" [kmdAddr]
    let sk = H.prvKeySecKey $ (fromString wif :: H.PrvKey)

    node <- liftIO $ spawnConsensusNode consensusConfig
    let config = EthNotariser bitcoinConf node gethConfig mandateAddr sk
    hathReader (const config) ethNotariser


-- Run configured notariser
--
ethNotariser :: Hath EthNotariser ()
ethNotariser = do
  ident@(wif, pk, myAddr) <- getBitcoinIdent
  logInfo $ "My bitcoin addr: " ++ show myAddr
  monitorUTXOs kmdInputAmount 5 ident
  run $ do
    mutxo <- chooseUtxo <$> bitcoinUtxos [myAddr]
    case mutxo of
         Nothing -> do
           logInfo "Waiting for UTXOs"
           liftIO $ threadDelay $ 60 * 1000000
         Just utxo -> do
           mblocks <- getBlocksToNotarise
           case mblocks of
                Nothing -> do
                  logInfo "Waiting for more blocks to notarise"
                  liftIO $ threadDelay $ 60 * 1000000
                Just blocks -> do
                  let ndata = getNotarisationData blocks
                  runNotariserConsensus utxo ndata
  where
    onConsensusExc e = do
      runHath () $ logInfo $ show (e :: ConsensusException)
      threadDelay $ 5 * 1000000
    onError e = do
      runHath () $ logError $ show (e :: SomeException)
      threadDelay $ 30 * 1000000
    run act = do
      r <- ask
      _ <- liftIO $ forever $ runHath r act `catch` onConsensusExc
                                            `catchAny` onError
      pure ()

-- Run consensus if UTXO and block limits are available
--
runNotariserConsensus :: BitcoinUtxo -> NotarisationData Sha3 -> Hath EthNotariser ()
runNotariserConsensus utxo ndata = do
  (_, members) <- asks getMandate >>= mandateGetMembers
  (wif, pk, kmdAddr) <- getBitcoinIdent
  sk <- asks getSecret
  let ident = (sk, pubKeyAddr $ derivePubKey sk)
  let cparams = ConsensusParams members ident consensusTimeout
  r <- ask
  let run = liftIO . runHath r
  let opret = Ser.encode ndata

  runConsensus cparams opret $ do

    -- Step 1 - Key on opret, collect UTXOs
    run $ logDebug "Step 1: Collect UTXOs"
    utxoBallots <- step waitMajority (pk, getOutPoint utxo)

    -- Step 2 - TODO: Key on proposer
    run $ logDebug "Step 2: Get proposed UTXOs"
    utxosChosen <- propose $ pure $ proposeInputs utxoBallots

    -- Step 3 - Sign tx and collect signed inputs
    run $ logDebug "Step 3: Sign & collect"
    let signedTx = signMyInput wif utxosChosen $ H.DataCarrier opret
        myInput = getMyInput utxo signedTx
        waitSigs = waitOutpoints $ snd <$> utxosChosen
    allSignedInputs <- step waitSigs myInput
    let finalTx = compileFinalTx signedTx allSignedInputs

    -- Step 4 - Confirm step 3 (bad attempt to overcome two general's problem)
    run $ logDebug "Step 4: Just for kicks"
    _ <- step waitMajority ()

    run $ logInfo $ "Broadcast transaction: " ++ show (H.txHash finalTx)
    run $ bitcoinSubmitTxSync finalTx

    liftIO $ threadDelay 10000000

getBlocksToNotarise :: Hath EthNotariser (Maybe [EthBlock])
getBlocksToNotarise = do
  mRange <- getBlockRange
  forM mRange $ \(from, to) -> do
    logInfo $ "Notarising range: " ++ show (from, to)
    forM [from..to] $ \n -> eth_getBlockByNumber n False

getBlockRange :: Hath EthNotariser (Maybe (U256, U256))
getBlockRange = do
  mlastNota <- getLastNotarisation "TESTETH"
  case mlastNota of
       Nothing -> do
         logInfo $ "No prior notarisations found"
         pure $ Just (end, end)
       Just (NOR{..}) -> do
         let start = fromIntegral blockNumber + 1
         end <- getEthProposeHeight 10
         if start < end
            then pure $ Just (start, end)
            else do
              logInfo $ "Waiting for more new blocks: " ++ show (start, end)


getLastNotarisation :: Has BitcoinConfig r => String -> Hath r (Maybe (NotarisationData Sha3))
getLastNotarisation symbol = do
  traceE "getLastNotarisation" $ do
    val <- queryBitcoin "scanNotarisationsDB" ["0", symbol, "10000"]
    pure $ if val == Null
              then Nothing
              else do
                let bs = unHex $ val .! "{opreturn}" :: ByteString
                let Right out = Ser.decode bs
                 in Just out

getEthProposeHeight :: Has GethConfig r => U256 -> Hath r U256
getEthProposeHeight n = do
  height <- eth_blockNumber
  pure $ height - mod height n

waitOutpoints :: [H.OutPoint] -> Waiter (Maybe H.TxIn)
waitOutpoints given = waitGeneric test
  where test _ inv =
          let getOPs = map H.prevOutput . catMaybes . map snd
              vals = getOPs $ Map.elems inv
           in sortOn show vals == sortOn show given

getNotarisationData :: [EthBlock] -> NotarisationData Sha3
getNotarisationData blocks =
  let notarised = last blocks
      mom = trieRoot $ receiptsRootTrieTrie blocks
  
   in NOR (ethBlockHash notarised)
          (fromIntegral $ ethBlockNumber notarised)
          "TESTETH"
          mom
          (fromIntegral $ length blocks)
          kmdCCid
  where
    receiptsRootTrieTrie headers =
      let heights = ethBlockNumber <$> headers
          roots = ethBlockReceiptsRoot <$> headers
          keys = rlpSerialize . rlpEncode . unU256 <$> heights
       in mapToTrie $ zip keys $ unHex <$> roots

-- Building KMD Notarisation TX -----------------------------------------------

proposeInputs :: [Ballot (H.PubKey, H.OutPoint)] -> [(H.PubKey, H.OutPoint)]
proposeInputs ballots =
  take notaryTxSigs $ bData <$> sortOn bSig ballots

getBitcoinIdent :: Hath EthNotariser BitcoinIdent
getBitcoinIdent = deriveBitcoinIdent <$> asks getSecret

chooseUtxo :: [BitcoinUtxo] -> Maybe BitcoinUtxo
chooseUtxo = listToMaybe . choose
  where
    choose = reverse . sortOn (\c -> (utxoConfirmations c, utxoTxid c))
                     . filter ((==kmdInputAmount) . utxoAmount)

signMyInput :: H.PrvKey -> [(H.PubKey, H.OutPoint)] -> H.ScriptOutput -> H.Tx
signMyInput wif ins opret = do
  let toSigIn (a, o) = H.SigInput (H.PayPK a) o (H.SigAll False) Nothing
      inputs = toSigIn <$> ins
      outputAmount = kmdInputAmount -- TODO: calc based on inputs
      outputs = [(notarisationRecip, outputAmount), (opret, 0)]
      etx = H.buildTx (H.sigDataOP <$> inputs) outputs
      signTx tx = H.signTx tx inputs [wif]
   in either error id $ etx >>= signTx

getMyInput :: BitcoinUtxo -> H.Tx -> Maybe H.TxIn
getMyInput myUtxo tx =
  find (\txIn -> H.prevOutput txIn == getOutPoint myUtxo)
       (H.txIn tx)

compileFinalTx :: H.Tx -> [Ballot (Maybe H.TxIn)] -> H.Tx
compileFinalTx tx ballots = tx { H.txIn = mergedIns }
  where
    signedIns = catMaybes $ bData <$> ballots
    unsignedIns = H.txIn tx
    mischief = throw $ ConsensusMischief $ "compileFinalTx: " ++ show (tx, ballots)
    mergedIns = map combine unsignedIns
    combine unsigned =
      case find (\a -> H.prevOutput a == H.prevOutput unsigned) signedIns of
           Nothing -> mischief
           Just signed -> unsigned { H.scriptInput = H.scriptInput signed }

