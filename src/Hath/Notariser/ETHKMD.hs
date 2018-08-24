
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

kmdCCid :: Word16
kmdCCid = 2

notarisationRecip :: H.ScriptOutput
notarisationRecip = H.PayPKHash $ H.getAddrHash "RXL3YXG2ceaB6C5hfJcN4fvmLH2C34knhA"

consensusTimeout :: Int
consensusTimeout = 10 * 1000000


data EthNotariser = EthNotariser
  { getKomodoConfig :: BitcoinConfig
  , getMandate :: Mandate
  , getNode :: ConsensusNode
  , getHathConfig :: HathConfig
  }

instance Has GethConfig    EthNotariser where has = has . getHathConfig
instance Has BitcoinConfig EthNotariser where has = getKomodoConfig
instance Has Mandate       EthNotariser where has = getMandate
instance Has ConsensusNode EthNotariser where has = getNode


-- Entry point for ETH notariser programme
runEthNotariser :: Maybe Address -> HathConfig -> IO ()
runEthNotariser maddress hathConfig = do
  threadDelay 2000000
  liftIO $ initKomodo
  runHath hathConfig $ do
    mandate <- loadMandate ethKmd maddress
    bitcoinConf <- loadBitcoinConfig "~/.komodo/komodo.conf"
    let (seed, port) = configVal hathConfig .! "{ETHKMD:{seed,port}}"
    node <- liftIO $ spawnConsensusNode seed port
    let config = EthNotariser bitcoinConf mandate node
    hathReader config ethNotariser


-- Run configured notariser
--
ethNotariser :: Hath EthNotariser ()
ethNotariser = do
  forever $ do
    ident@(wif, myAddr) <- getBitcoinIdent
    logInfo $ "My bitcoin addr: " ++ show myAddr
    mutxo <- chooseUtxo <$> bitcoinUtxos [myAddr]
    case mutxo of
         Nothing -> do
           logWarn "Out of UTXOs..."
           liftIO $ threadDelay $ 10 * 60 * 1000000
         Just utxo -> do
           mlimits <- getBlockLimits
           case mlimits of
                Nothing -> do
                  liftIO $ threadDelay $ 60 * 1000000
                Just limits -> runNotariserConsensus utxo limits


-- Run consensus if UTXO and block limits are available
runNotariserConsensus :: BitcoinUtxo -> (Integer, Integer) -> Hath EthNotariser ()
runNotariserConsensus utxo range = do
  (_, members) <- mandateGetMembers
  (_, kmdAddr) <- getBitcoinIdent
  ident <- asks $ getMe . has
  let cparams = ConsensusParams members ident consensusTimeout
  r <- ask
  let run = liftIO . runHath r

  runConsensus cparams range $ do

    -- Step 1 - Key on opret, collect UTXOs
    run $ logDebug "Step 1: Collect UTXOs"
    results1 <- step $ Ser2Bin (kmdAddr, getOutPoint utxo)

    run $ logDebug "Step 2: Get proposed transaction"
    -- Step 2 - Key on (opret, proposer), get proposed transaction
    let utxoBallots = [b { bData = unSer2Bin $ bData b } | b <- results1]
        ptx = Ser2Bin $ proposeTransaction utxoBallots
    tx <- unSer2Bin <$> propose (pure ptx)

    -- Step 3 - 
    run $ logDebug "Step 3: TBD"
    ifProposer $ do
      run $ logInfo "I am proposer - not sending TX"
    run $ waitTx $ tx


waitTx :: H.Tx -> Hath EthNotariser ()
waitTx tx = do
  liftIO $ threadDelay 10000000
  error "what now?"

getBlockLimits :: Hath EthNotariser (Maybe (Integer, Integer))
getBlockLimits = do
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


-- Building KMD Notarisation TX -----------------------------------------------

proposeTransaction :: [Ballot (H.Address, H.OutPoint)] -> H.Tx
proposeTransaction ballots =
  let toSigIn (a, o) = H.SigInput (H.PayPKHash $ H.getAddrHash a) o (H.SigAll False) Nothing
      inputs = take notaryTxSigs $ toSigIn . bData <$> sortOn bSig ballots
      outputAmount = round (kmdInputAmount/100*1e8) -- TODO: calc based on inputs
      outputs = [(notarisationRecip, outputAmount)] -- TODO: , (opRet, 0)]
   in either error id $ H.buildTx (H.sigDataOP <$> inputs) outputs


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
