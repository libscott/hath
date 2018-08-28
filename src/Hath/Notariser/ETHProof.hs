
module Hath.Notariser.ETHProof where

import Network.Ethereum

import Hath.Data.Aeson
import Hath.Config
import Hath.Concurrent
import Hath.Prelude

-- :main prove ethkmd 0x80703eba27d9e0c6b1e19ef847d3b4aab7f04da4c510639c742591f8eef724e2



proveReceiptInBlock :: Has GethConfig r =>
    TransactionReceipt -> EthBlock -> Hath r Trie
proveReceiptInBlock receipt block = do
  let hashes = ethBlockTransactions block
  allReceipts <- do
    logTime ("get receipts " ++ show (length hashes)) $ do
      parM 10 hashes eth_getTransactionReceipt

  pure $ stringsToTrie $ rlpSerialize . rlpEncode <$> allReceipts






proveEthKmdTransaction :: GethConfig -> Sha3 -> IO ()
proveEthKmdTransaction gethConf txid = do
  runHath gethConf $ do
    receipt <- eth_getTransactionReceipt txid
    block <- eth_getBlockByHash $ blockHash receipt

    trie <- proveReceiptInBlock receipt block


    logInfo $ show $ trieRoot trie
    logInfo $ show $ block




    --block <- getBlock (block receit) True
    -- make proof in block
    
    --findNotarisationIncludingBlock
    --getRange
    --makeProofOfBlockInRange


--getNotarisationOnKmd :: Word32 -> String -> Maybe (NotarisationData Sha3)
--getNotarisationOnKmd height symbol = do
--  -- Gonna do a nice binary search here
--  let search low high = do
--        let mid = quot $ (low + high) / 2
--    
--
--
--
--  let low = 980000
--  search low 0
--
--  n <- findNotarisation 0 height
--  if blockNumber n < height





