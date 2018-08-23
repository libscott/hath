{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Hath.Consensus
  ( Consensus
  , ConsensusNode(..)
  , ConsensusParams(..)
  , Ballot(..)
  , spawnConsensusNode
  , runConsensus
  , step
  , propose
  , ifProposer
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
import           Hath.Consensus.Step


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

data ConsensusException = ConsensusTimeout | ConsensusProposalMissing
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
    runProcess node $ act' >>= putMVar handoff
    takeMVar handoff

step' :: Serializable a => a -> Consensus (Inventory a)
step' obj = do
  topic <- permuteTopic
  ConsensusParams members (sk, myAddr) timeout <- ask
  let ballot = Ballot myAddr (sign sk topic) obj
  lift $ lift $ do
    (send, recv) <- newChan
    _ <- spawnLocal $ runStep topic ballot members $ sendChan send
    waitMajority recv timeout members

step :: Serializable a => a -> Consensus [Ballot a]
step o = do
  r <- Map.toAscList <$> step' o
  pure [Ballot a s o | (a, (s, o)) <- r]

propose :: Serializable a => Consensus a -> Consensus a
propose mObj = do
  (pAddr, isMe) <- getProposer
  obj <- if isMe then Just <$> mObj else pure Nothing
  results <- step' obj
  case Map.lookup pAddr results of
       Just (_, Just obj) -> pure obj
       _                  -> throw ConsensusProposalMissing

getProposer :: Consensus (Address, Bool)
getProposer = do
  -- This gives fairly good distribution:
  -- import hashlib
  -- dist = [0] * 64
  -- for i in xrange(1000000):
  --     m = hashlib.sha256(str(i))
  --     d = sum(map(ord, m.digest()))
  --     distribution[d%64] += 1
  -- print dist
  ConsensusParams members (_, myAddr) _ <- ask
  let msg2sum = sum . map fromIntegral . BS.unpack . getMsg
  topic <- get
  let p = mod (msg2sum topic) (length members)
      pAddr = members!!p
  pure $ (pAddr, pAddr == myAddr)

ifProposer :: Consensus () -> Consensus ()
ifProposer act = do
  (_, isMe) <- getProposer
  when isMe act

permuteTopic :: Consensus Topic
permuteTopic = do
  out <- get
  put $ hashMsg $ getMsg out
  pure out

-- Process ----

waitMajority :: Serializable a => ReceivePort (Inventory a) -> Timeout
             -> [Address] -> Process (Inventory a)
waitMajority recv timeout members = do
  startTime <- liftIO getCurrentTime
  fix $ \f -> do
    t <- diffUTCTime startTime <$> liftIO getCurrentTime
    let us = max 0 $ round $ (realToFrac t) * 1000000 + fromIntegral timeout
    mballots <- receiveChanTimeout us recv
    case mballots of
         Nothing -> throw ConsensusTimeout
         Just ballots | haveMajority members ballots -> pure ballots
         _ -> f

haveMajority :: [Address] -> Inventory a -> Bool
haveMajority members ballots =
  let required = quot (length members) 3 + 1
   in length ballots >= required

