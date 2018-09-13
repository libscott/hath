{-# LANGUAGE NamedFieldPuns #-}

module Hath.Notariser.ETHProof where

import Control.Monad.Trans.Maybe

import qualified Data.ByteString as BS
import Data.Serialize

import Network.Bitcoin
import Network.Ethereum
import Network.Komodo hiding (blockHash)
import qualified Network.Haskoin.Internals as H

import Hath.Data.Aeson
import Hath.Config
import Hath.Concurrent
import Hath.Notariser.Types
import Hath.Prelude
import Debug.Trace

-- prove ethkmd TXSCL-ETH-STAGING 0x8b01463206e0157bc43c7212a8597349e92d3b744bd3b8fa91c1ee6423e08f1e

kmdImportErc20 = 10^9+1
kmdImportCoin = "\xe2"


data EthProver = EthProver GethConfig BitcoinConfig
instance Has GethConfig EthProver where has (EthProver g _) = g
instance Has BitcoinConfig EthProver where has (EthProver _ b) = b

runProveEthKmdTransaction :: GethConfig -> String -> (Hath EthProver ByteString) -> IO ()
runProveEthKmdTransaction gc bcc act = do
  conf <- EthProver gc <$> loadBitcoinConfig bcc
  runHath conf act >>= print . toHex

proveEthKmdTransaction :: String -> Sha3 -> H.Tx -> Hath EthProver ByteString
proveEthKmdTransaction targetSymbol txid importTx = do
  when (H.txIn importTx /= []) $ do
    error "Import tx cannot have inputs"

  receipt <- eth_getTransactionReceipt txid
  let (tokenAddress, amount, to) =
        case getErc20Details receipt of
          Right d -> d
          Left s -> error $ "Counldn't read ERC20: " ++ s

  logInfo $ "ERC20 contract: " ++ show tokenAddress
  logInfo $ "Amount: " ++ show amount

  --when (not $ checkBurn importTx to) $ do
  --  error "Import tx does not match burn address"
  
  logInfo "Create trie receipt->block"
  mp1 <- proveEthReceiptInBlock receipt

  logInfo "Create trie block->MoM"
  (mp2, notarisationTxid) <- proveKmdCrossChain1 receipt targetSymbol

  let reference = fromShort $ H.getHash256 $ H.getTxHash notarisationTxid
      payload = rlpSerialize $ rlpEncode receipt
      packet = ProofPacket payload reference [mp1, mp2]

      opret = H.DataCarrier $ rlpSerialize $ rlpEncode packet
      outs = H.TxOut 0 (H.encodeOutputBS opret) : H.txOut importTx

      txid' = fromString $ case show txid of
                             ('0':'x':xs) -> xs
                             xs           -> xs
      prevout = H.OutPoint txid' kmdImportErc20
      input = H.TxIn prevout kmdImportCoin 0

  pure $ encode $ importTx {H.txIn=[input], H.txOut=outs}

proveEthReceiptInBlock :: TransactionReceipt -> Hath EthProver MerkleProof
proveEthReceiptInBlock receipt = do
  block <- eth_getBlockByHash $ blockHash receipt
  trie <- getBlockReceiptsTrie block

  let pos = fromIntegral $ transactionIndex receipt
      key = orderedTrieKey pos
      branch = trieProof key (Leaf [] "") trie
      payload = rlpSerialize $ rlpEncode receipt
      rhash = execTrieProof key (Leaf [] payload) branch
      invalidProof = rhash /= ethBlockReceiptsRoot block

  when invalidProof $ error "Could not construct proof tx->block"
  pure $ TrieMerkleProof (BlockPosition pos) branch

proveKmdCrossChain1 :: TransactionReceipt -> Symbol -> Hath EthProver (MerkleProof, H.TxHash)
proveKmdCrossChain1 receipt targetSymbol = do
  block <- eth_getBlockByHash $ blockHash receipt
  -- now there is a proof leading to the root of the block
  -- need a proof leading to the root of the MoM
  -- for this, we binary search the notarisations on kmd
  -- to find the range of blocks that were notarised
  let receiptHeight = fromIntegral $ ethBlockNumber block
  mnota <- findNotarisation targetSymbol $ fromIntegral receiptHeight
  let nota = maybe (error "Could not find notarisation for block") id mnota
      Notarisation kmdHeight notarisationTxid ndata = nota
  -- get all the block headers in order to reconstruct the MoM
  blocks <- getBlocksForNotarisation ndata
  let trie = ethTrieTrie blocks
      key = toNibbles $ ethMomKey receiptHeight
      branch = trieProof key (Leaf [] "") trie
      rhash = execTrieProof key (Leaf [] $ unSha3 $ ethBlockReceiptsRoot block) branch
      invalidProof = rhash /= mom ndata

  when invalidProof $ error "Could not construct proof block->mom"

  let proof = TrieMerkleProof (CrossChain targetSymbol receiptHeight) branch
  pure (proof, notarisationTxid)

getErc20Details :: TransactionReceipt -> Either String (Address, U256, Address)
getErc20Details Receipt{logs=[LogEntry{address,topics=[_,_,t2],_data}]} = do
  (,,) address <$> decodeABI (unHex _data) <*> decodeABI (unSha3 t2)
getErc20Details _ = error "Doesn't look like an ERC20 transaction"

checkBurn :: H.Tx -> Address -> Bool
checkBurn importTx burnAddr =
  let txbs = fromShort $ H.getHash256 $ H.getTxHash $ H.txHash importTx
   in BS.drop 12 txbs == fromAddress burnAddr

getBlockReceiptsTrie :: EthBlock -> Hath EthProver Trie
getBlockReceiptsTrie block = do
  let hashes = ethBlockTransactions block
  allReceipts <- do
    logTime ("get receipts " ++ show (length hashes)) $ do
      parM 10 hashes eth_getTransactionReceipt
  pure $ orderedTrie $ rlpSerialize . rlpEncode <$> allReceipts

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

ethMomKey :: Integer -> ByteString
ethMomKey h = rlpSerialize $ rlpEncode (h, "Receipts"::ByteString)

ethTrieTrie :: [EthBlock] -> Trie
ethTrieTrie headers =
  let items header = (:[])
        ( ethMomKey $ unU256 $ ethBlockNumber header
        , unSha3 $ ethBlockReceiptsRoot header
        )
   in mapToTrie $ headers >>= items
