
module Hath.Notariser.ETHProof where

import Network.Ethereum.Crypto
import Network.Ethereum.Data
import Network.Ethereum.RPC

import Hath.Data.Aeson
import Hath.Config
import Hath.Prelude


proveEthKmdTransaction :: GethConfig -> Sha3 -> IO ()
proveEthKmdTransaction gethConf txid = do
  runHath gethConf $ do
    receipt <- queryEthereum "eth_getTransactionByHash" [txid]
    logInfo $ show (receipt :: Value)

    block <- queryEthereum "eth_getBlockByNumber" (6215357::U256, False)
    logInfo $ asString (block::Value)
    --{"hash":"0x1f38df2a4ef1855ca78bd73c65b6ab14401fec2957e78af814f6bc51d7d733e0","gas":"0x186a0","gasPrice":"0x59682f00","to":"0x83fa34ffd45dced0482d92048d04823556351520","value":"0x0","blockHash":"0x83c075279abe4c0960695e40257b24fb999f8c0aa83ea74e5baa75a5a4026067","input":"0x","from":"0x1b4e723ab980318b796d9a1be586ce12fcd22673","blockNumber":"0x5ed6bd","transactionIndex":"0x79","r":"0x1ac2655c8fd36bdb4df29af846414fbe220dcf883b310a4cb133c9725ed77146","s":"0x2c02dd1e1ef63ddd6d44e327b06139463e071c5df80e5df212ee58906cfaa414","v":"0x26","nonce":"0x2"},

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





