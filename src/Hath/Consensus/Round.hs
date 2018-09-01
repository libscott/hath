{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hath.Consensus.Round
  ( ConsensusException(..)
  , step
  , propose
  , say
  , waitMembers
  , waitMajority
  , haveMajority
  , waitGeneric
  , runConsensus
  , inventoryIndex
  , prioritiseRemoteInventory
  , dedupeInventoryQueries
  , majorityThreshold
  ) where

import           Control.Monad.Reader
import           Control.Monad.State

import           Control.Distributed.Process hiding (catch)
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable (Serializable)

import qualified Data.ByteString as BS
import qualified Data.Binary as Bin
import qualified Data.Map as Map

import           Network.Ethereum.Crypto

import           Hath.Prelude
import           Hath.Prelude.Lifted
import qualified Hath.Consensus.P2P as P2P
import           Hath.Consensus.Step
import           Hath.Consensus.Types
import           Hath.Consensus.Utils


-- Run round ------------------------------------------------------------------

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

-- 1. Determine a starting proposer
-- 2. Try to get a proposal from them
-- 3. If there's a timeout, move to the next proposer
propose :: forall a. Serializable a => Consensus a -> Consensus a
propose mObj = do
  determineProposers >>= go
    where
      go :: [(Address, Bool)] -> Consensus a
      go [] = throw ConsensusTimeout
      go ((pAddr, isMe):xs) = do

        let nextProposer ConsensusTimeout = do
              _ <- permuteTopic  -- Will have been lost due to exception
              lift $ lift $ say $ "Timeout waiting for proposer: " ++ show pAddr
              go xs
            nextProposer e = throw e

        obj <- if isMe then Just <$> mObj else pure Nothing

        handle nextProposer $ do
          results <- step' (waitMembers [pAddr]) obj
          case Map.lookup pAddr results of
               Just (_, Just obj2) -> pure obj2
               _                   -> do
                 lift $ lift $ say $ "Mischief: missing proposal from: " ++ show pAddr
                 throw ConsensusTimeout

determineProposers :: Consensus [(Address, Bool)]
determineProposers = do
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
  let i = mod (msg2sum topic) (length members)
      proposers = take (length members) $ drop i $ cycle members
  pure $ [(p, p == myAddr) | p <- proposers]

permuteTopic :: Consensus Topic
permuteTopic = do
  out <- get
  put $ hashMsg $ getMsg out
  pure out

-- Check Majority -------------------------------------------------------------

waitGeneric :: Serializable a => ([Address] -> Inventory a -> Bool) -> Waiter a
waitGeneric test recv timeout members = do
  startTime <- getCurrentTime
  fix $ \f -> do
    d <- timeDelta startTime
    let us = max 0 $ timeout - d
    minv <- receiveChanTimeout us recv
    case minv of
         Nothing -> throw ConsensusTimeout
         Just inv | test members inv -> pure inv
         _ -> f

waitMembers :: Serializable a => [Address] -> Waiter a
waitMembers addrs = waitGeneric $ \_ inv -> allSigned inv
  where allSigned inv = all id [Map.member a inv | a <- addrs]

waitMajority :: Serializable a => Waiter a
waitMajority = waitGeneric haveMajority

haveMajority :: [Address] -> Inventory a -> Bool
haveMajority members inv =
   length inv >= majorityThreshold (length members)

majorityThreshold :: Int -> Int
majorityThreshold m = floor $ (fromIntegral m) / 2 + 1
