
module Hath.Notariser.ETHProof where

import Control.Monad.Trans.Maybe

import Network.Bitcoin
import Network.Ethereum
import Network.Komodo hiding (blockHash)

import Hath.Config
import Hath.Concurrent
import Hath.Prelude


data EthProver = EthProver GethConfig BitcoinConfig
instance Has GethConfig EthProver where has (EthProver g _) = g
instance Has BitcoinConfig EthProver where has (EthProver _ b) = b

runProveEthKmdTransaction :: GethConfig -> String -> Sha3 -> IO ()
runProveEthKmdTransaction gc bcc txid = do
  conf <- EthProver gc <$> loadBitcoinConfig bcc
  runHath conf $ proveEthKmdTransaction txid

proveEthKmdTransaction :: Sha3 -> Hath EthProver ()
proveEthKmdTransaction txid = do
  receipt <- eth_getTransactionReceipt txid
  block <- eth_getBlockByHash $ blockHash receipt
  trie <- getBlockReceiptsTrie block

  let key = orderedTrieKey $ transactionIndex receipt
      proof = trieProof key "" trie
      payload = rlpSerialize $ rlpEncode receipt
      rhash = execTrieProof key payload proof
      invalidProof = rhash /= ethBlockReceiptsRoot block

  when invalidProof $ error "Could not construct proof tx->block"

  -- now there is a proof leading to the root of the block
  -- need a proof leading to the root of the MoM
  -- for this, we b-search the notarisations on kmd
  let receiptHeight = fromIntegral $ ethBlockNumber block
  mndata <- findNotarisation "TXSCL-ETH-STAGING" receiptHeight
  let ndata = maybe (error "Could not find notarisaton for block") id mndata
  -- get all the block headers in order to reconstruct the MoM
  blocks <- getBlocksForNotarisation ndata
  let trie2 = receiptsRootTrieTrie blocks
      key2 = orderedTrieKey receiptHeight
      proof2 = trieProof key2 "" trie2
      rhash2 = execTrieProof key2 (unSha3 $ ethBlockReceiptsRoot block) proof2
      invalidProof2 = rhash2 /= mom ndata

  when invalidProof2 $ error "Could not construct proof block->mom"

  -- merge the proofs
  liftIO $ print (key, key2, proof, proof2, payload)

getBlockReceiptsTrie :: EthBlock -> Hath EthProver Trie
getBlockReceiptsTrie block = do
  let hashes = ethBlockTransactions block
  allReceipts <- do
    logTime ("get receipts " ++ show (length hashes)) $ do
      parM 10 hashes eth_getTransactionReceipt
  pure $ orderedTrie $ rlpSerialize . rlpEncode <$> allReceipts

findNotarisation :: String -> Word32 -> Hath EthProver (Maybe (NotarisationData Sha3))
findNotarisation symbol height = do
  let currentHeight = fromIntegral <$> bitcoinGetHeight
  f 987000 <$> currentHeight >>= runMaybeT
  where
    f :: Word32 -> Word32 -> MaybeT (Hath EthProver) (NotarisationData Sha3)
    f begin end
      | begin == end = MaybeT $ pure Nothing
      | otherwise = do
          let mid = quot (begin+end) 2
          ndata <- MaybeT $ scanNotarisationsDB mid symbol 100000
          case cmpRange ndata of
               LT -> f begin mid
               GT -> f mid end
               EQ -> pure ndata
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
