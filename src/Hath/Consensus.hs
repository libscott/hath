{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Hath.Consensus
  ( Consensus
  , ConsensusNode(..)
  , ConsensusParams(..)
  , ConsensusException(..)
  , Ballot(..)
  , Waiter
  , spawnConsensusNode
  , runConsensus
  , step
  , propose
  , ifProposer
  , say
  , waitMembers
  , waitMajority
  , haveMajority
  , waitGeneric
  ) where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State

import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable (Serializable)
import           Control.Monad.Trans.Maybe

import qualified Data.ByteString as BS
import qualified Data.Binary as Bin
import qualified Data.Map as Map
import           Data.Time.Clock

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC

import           GHC.Generics (Generic)

import           Hath.Prelude
import           Hath.Lifted
import qualified Hath.Consensus.P2P as P2P
import           Hath.Consensus.Process


-- Node -----------------------------------------------------------------------

data ConsensusNode = ConsensusNode LocalNode

spawnConsensusNode :: String -> String -> IO ConsensusNode
spawnConsensusNode seed port = do
  let host = "localhost"
      ext = const (host, port)
      seeds = [P2P.makeNodeId seed]
  (node, _) <- P2P.startP2P host port ext initRemoteTable seeds
  pure $ ConsensusNode node

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

runConsensus :: (Serializable a, Has ConsensusNode r) => ConsensusParams
             -> a -> Consensus b -> Hath r b
runConsensus params topicData act = do
  let topic = hashMsg $ toStrict $ Bin.encode topicData
      act' = runReaderT (evalStateT act topic) params
  ConsensusNode node <- asks $ has
  liftIO $ do
    handoff <- newEmptyMVar
    runProcess node $ do
      _ <- spawnLocalLink P2P.peerNotifier
      act' >>= putMVar handoff
    takeMVar handoff

-- Coordinate Round -----------------------------------------------------------

step' :: Serializable a => Waiter a -> a -> Consensus (Inventory a)
step' waiter obj = do
  topic <- permuteTopic
  ConsensusParams members (sk, myAddr) timeout <- ask
  let ballot = Ballot myAddr (sign sk topic) obj
  lift $ lift $ do
    (send, recv) <- newChan
    _ <- spawnLocalLink $ runStep topic ballot members $ sendChan send
    waiter recv timeout members

step :: Serializable a => Waiter a -> a -> Consensus [Ballot a]
step waiter o = do
  r <- Map.toAscList <$> step' waiter o
  pure [Ballot a s o | (a, (s, o)) <- r]

propose :: Serializable a => Consensus a -> Consensus a
propose mObj = do
  (pAddr, isMe) <- determineProposer
  obj <- if isMe then Just <$> mObj else pure Nothing
  results <- step' (waitMembers [pAddr]) obj
  case Map.lookup pAddr results of
       Just (_, Just obj) -> pure obj
       _                  -> throw ConsensusProposalMissing

determineProposer :: Consensus (Address, Bool)
determineProposer = do
  {- This gives fairly good distribution:
  import hashlib
  dist = [0] * 64
  for i in xrange(1000000):
      m = hashlib.sha256(str(i))
      d = sum(map(ord, m.digest()))
      distribution[d%64] += 1
  print dist
  -}
  ConsensusParams members (_, myAddr) _ <- ask
  let msg2sum = sum . map fromIntegral . BS.unpack . getMsg
  topic <- get
  let p = mod (msg2sum topic) (length members)
      pAddr = members!!p
  pure $ (pAddr, pAddr == myAddr)

ifProposer :: Consensus () -> Consensus ()
ifProposer act = do
  (_, isMe) <- determineProposer
  when isMe act

permuteTopic :: Consensus Topic
permuteTopic = do
  out <- get
  put $ hashMsg $ getMsg out
  pure out

-- Process ----

type Waiter a = ReceivePort (Inventory a) -> Timeout -> [Address] -> Process (Inventory a)

waitGeneric :: Serializable a => ([Address] -> Inventory a -> Bool) -> Waiter a
waitGeneric test recv timeout members = do
  startTime <- liftIO getCurrentTime
  fix $ \f -> do
    t <- diffUTCTime startTime <$> liftIO getCurrentTime
    let us = max 0 $ round $ (realToFrac t) * 1000000 + fromIntegral timeout
    mballots <- receiveChanTimeout us recv
    case mballots of
         Nothing -> throw ConsensusTimeout
         Just ballots | test members ballots -> pure ballots
         _ -> f

waitMembers :: Serializable a => [Address] -> Waiter a
waitMembers addrs = waitGeneric $ \_ inv -> allSigned inv
  where allSigned inv = all id [Map.member a inv | a <- addrs]

waitMajority :: Serializable a => Waiter a
waitMajority = waitGeneric haveMajority

haveMajority :: [Address] -> Inventory a -> Bool
haveMajority members ballots =
  let m = fromIntegral $ length members
      required = floor (m / 3 * 2) + 1
   in length ballots >= required
