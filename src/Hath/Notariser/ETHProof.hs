
module Hath.Notariser.ETHProof where

import Network.Ethereum

import Hath.Config
import Hath.Concurrent
import Hath.Prelude

-- :main prove ethkmd 0x80703eba27d9e0c6b1e19ef847d3b4aab7f04da4c510639c742591f8eef724e2



getBlockReceiptsTrie :: Has GethConfig r => EthBlock -> Hath r Trie
getBlockReceiptsTrie block = do
  let hashes = ethBlockTransactions block
  allReceipts <- do
    logTime ("get receipts " ++ show (length hashes)) $ do
      parM 10 hashes eth_getTransactionReceipt

  pure $ orderedTrie $ rlpSerialize . rlpEncode <$> allReceipts


proveEthKmdTransaction :: GethConfig -> Sha3 -> IO ()
proveEthKmdTransaction gethConf txid = do
  runHath gethConf $ do
    receipt <- eth_getTransactionReceipt txid
    block <- eth_getBlockByHash $ blockHash receipt

    trie <- getBlockReceiptsTrie block


    logInfo $ show $ trieRoot trie
    logInfo $ show $ block

