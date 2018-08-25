{-# LANGUAGE DeriveGeneric #-}

module Hath.Consensus.Types where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Network.Ethereum.Crypto
import           GHC.Generics (Generic)
import Hath.Prelude
import Hath.Lifted
import Data.Binary
import Hath.Consensus.P2P as P2P

data ConsensusNode = ConsensusNode LocalNode

data Ballot a = Ballot
  { bMember :: Address
  , bSig :: CompactRecSig
  , bData :: a
  } deriving (Show, Generic)

instance Binary a => Binary (Ballot a)

type Authenticated a = (CompactRecSig, a)
type Inventory a = Map Address (CompactRecSig, a)
type Waiter a = ReceivePort (Inventory a) -> Timeout -> [Address] -> Process (Inventory a)

-- Params ---------------------------------------------------------------------

type Timeout = Int

data ConsensusParams = ConsensusParams
  { members' :: [Address]
  , getIdent' :: Ident
  , timeout' :: Timeout
  }

-- Monad ----------------------------------------------------------------------

type Topic = Msg
type Consensus = StateT Topic (ReaderT ConsensusParams Process)

data ConsensusException = ConsensusTimeout
                        | ConsensusProposalMissing
                        | ConsensusMischief String
  deriving (Show)
instance Exception ConsensusException
