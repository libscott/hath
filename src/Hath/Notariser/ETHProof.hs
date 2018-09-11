
module Hath.Notariser.ETHProof where

import Control.Monad.Trans.Maybe

import Network.Bitcoin
import Network.Ethereum
import Network.Komodo hiding (blockHash)
import Network.Haskoin.Internals as H (Hash256)

import Hath.Data.Aeson
import Hath.Config
import Hath.Concurrent
import Hath.Prelude
import Debug.Trace

-- prove 0x97116689accbbb49c8730d978bdac6afac5266c79620f309211fcc78fc77670c

data EthProver = EthProver GethConfig BitcoinConfig
instance Has GethConfig EthProver where has (EthProver g _) = g
instance Has BitcoinConfig EthProver where has (EthProver _ b) = b

runProveEthKmdTransaction :: GethConfig -> String -> (Hath EthProver ()) -> IO ()
runProveEthKmdTransaction gc bcc act = do
  conf <- EthProver gc <$> loadBitcoinConfig bcc
  runHath conf act

type SuperMoMData = [((ByteString, Integer, ByteString, Integer), ByteString)]

proveEthKmdTransaction :: Sha3 -> String -> Hath EthProver ()
proveEthKmdTransaction txid targetSymbol = do
  logInfo "Create trie receipt->block"
  receipt <- eth_getTransactionReceipt txid
  block <- eth_getBlockByHash $ blockHash receipt
  trie <- getBlockReceiptsTrie block


  let key = orderedTrieKey $ transactionIndex receipt
      proof = trieProof key "" trie
      payload = rlpSerialize $ rlpEncode receipt
      rhash = execTrieProof key payload proof
      invalidProof = rhash /= ethBlockReceiptsRoot block

  when invalidProof $ error "Could not construct proof tx->block"

  logInfo "Create trie block->MoM"
  -- now there is a proof leading to the root of the block
  -- need a proof leading to the root of the MoM
  -- for this, we binary search the notarisations on kmd
  -- to find the range of blocks that were notarised
  let receiptHeight = fromIntegral $ ethBlockNumber block
  mnota <- findNotarisation "TXSCL-ETH-STAGING" receiptHeight
  let nota = maybe (error "Could not find notarisation for block") id mnota
      Notarisation kmdHeight notarisationTxid ndata = nota
  -- get all the block headers in order to reconstruct the MoM
  blocks <- getBlocksForNotarisation ndata
  let trie2 = receiptsRootTrieTrie blocks
      key2 = orderedTrieKey receiptHeight
      proof2 = trieProof key2 "" trie2
      rhash2 = execTrieProof key2 (unSha3 $ ethBlockReceiptsRoot block) proof2
      invalidProof2 = rhash2 /= mom ndata

  when invalidProof2 $ error "Could not construct proof block->mom"

  logInfo "Create trie MoM->MoMoM"
  -- lastly, we need a proof leading to the backnotarisation on the target chain.
  -- We can calculate this from kmd, what we need to do is find the next notarisation
  -- for the target symbol after the notarisation from the source chain, ie, scan forwards.
  -- We can use findNotarisation again.

  r <- queryBitcoin "MoMoMdata" [targetSymbol, show (kmdHeight-1), "0"]
  let md = rlpDecode $ rlpDeserialize $ unHex $ r .! "{SuperMoMdata}" :: SuperMoMData
  liftIO $ print md
  liftIO $ print mnota




  
  
  
  --mnota2 <- scanForward kmdHeight targetSymbol
  --let nota2 = maybe (error "Could not find notarisation for block") id mnota
  --liftIO $ print mnota2

  -- merge the proofs
  --liftIO $ print (key, key2, proof, proof2, payload)

getBlockReceiptsTrie :: EthBlock -> Hath EthProver Trie
getBlockReceiptsTrie block = do
  let hashes = ethBlockTransactions block
  allReceipts <- do
    logTime ("get receipts " ++ show (length hashes)) $ do
      parM 10 hashes eth_getTransactionReceipt
  pure $ orderedTrie $ rlpSerialize . rlpEncode <$> allReceipts

-- Scan the index of notarisations forward until encounter a notarisation
-- including given height
scanForward :: Word32 -> String -> Hath EthProver (Maybe (NotarisationData H.Hash256))
scanForward h symbol = f [h..h+1440]
  where
    f [] = pure Nothing
    f (from:xs) = do
      scanNotarisationsDB from symbol 1 >>=
        \case Nothing -> f xs
              Just n  -> pure . Just $ opret n

findNotarisation :: String -> Word32 -> Hath EthProver (Maybe (Notarisation Sha3))
findNotarisation symbol height = do
  let currentHeight = fromIntegral <$> bitcoinGetHeight
  f 987000 <$> currentHeight >>= runMaybeT
  where
    f begin end
      | begin == end = MaybeT $ pure Nothing
      | otherwise = do
          let mid = quot (begin+end) 2
          nota <- MaybeT $ scanNotarisationsDB mid symbol 100000
          case cmpRange (opret nota) of
               LT -> f begin (mid-1)
               GT -> f (mid+1) end
               EQ -> pure nota
    cmpRange NOR{..}
      | height > blockNumber = GT
      | height < blockNumber - (fromIntegral momDepth) = LT
      | otherwise = EQ

getBlocksInRange :: Has GethConfig r => (U256, U256) -> Hath r [EthBlock]
getBlocksInRange (from, to) = do
  logTime ("eth_getBlockByNumber: " ++ show (from, to)) $ do
    parM 10 [from..to] $ \n -> do
        eth_getBlockByNumber n

getBlocksForNotarisation :: NotarisationData Sha3 -> Hath EthProver [EthBlock]
getBlocksForNotarisation NOR{..} = do
  let h = fromIntegral blockNumber
      d = fromIntegral momDepth
  getBlocksInRange (h - d + 1, h)

receiptsRootTrieTrie :: [EthBlock] -> Trie
receiptsRootTrieTrie headers =
  let heights = ethBlockNumber <$> headers
      roots = ethBlockReceiptsRoot <$> headers
      keys = rlpSerialize . rlpEncode . unU256 <$> heights
   in mapToTrie $ zip keys $ unSha3 <$> roots
