
module Hath.Consensus
  ( module Hath.Consensus.Types
  , spawnConsensusNode
  , runConsensus
  , propose
  , step
  , waitGeneric
  , waitMajority
  ) where


import Control.Distributed.Process.Node

import Hath.Consensus.Types
import Hath.Consensus.Process
import Hath.Consensus.Round
import Hath.Consensus.P2P as P2P


-- Node -----------------------------------------------------------------------


spawnConsensusNode :: String -> String -> IO ConsensusNode
spawnConsensusNode seed port = do
  let host = "localhost"
      ext = const (host, port)
      seeds = [P2P.makeNodeId seed]
  (node, _) <- P2P.startP2P host port ext initRemoteTable seeds
  pure $ ConsensusNode node


