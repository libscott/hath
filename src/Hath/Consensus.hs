
module Hath.Consensus
  ( module Hath.Consensus.Types
  , spawnConsensusNode
  , runConsensus
  , propose
  , step
  , waitGeneric
  , waitMajority
  , majorityThreshold
  ) where


import Control.Distributed.Process.Node

import Hath.Consensus.Types
import Hath.Consensus.Step
import Hath.Consensus.Round
import Hath.Consensus.P2P as P2P

import Hath.Config
import Hath.Prelude


-- Node -----------------------------------------------------------------------


spawnConsensusNode :: ConsensusNetworkConfig -> IO ConsensusNode
spawnConsensusNode CNC{..} = do
  let port' = show port
      ext = const (host, show port)
      seeds' = P2P.makeNodeId . addPort <$> seeds
      addPort s = if Nothing == elemIndex ':' s
                     then s ++ ":" ++ port'
                     else s
  (node, _) <- P2P.startP2P host port' ext initRemoteTable seeds'
  pure $ ConsensusNode node


