{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hath.Mandate.Consensus where

import           Control.Distributed.Process as CDP
import           Control.Distributed.Process.Serializable (Serializable)
import           Control.Monad.Trans.Maybe

import qualified Data.Map as Map

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC

import           Hath.Prelude
import           Hath.Lifted
import           Hath.Mandate.Round
import           Hath.Mandate.Types
import Debug.Trace


runConsensusRound :: (Has GethConfig r, Has Mandate r, Serializable a, Show a)
                  => Msg -> a -> RoundWaiter a b -> Hath r (Maybe b)
runConsensusRound message myData waiter = do
  logInfo $ "Round topic: " ++ show (toHex $ getMsg message)
  (sk, myAddr) <- asks $ getMe . has
  (_, members) <- mandateGetMembers
  let crs = sign sk message
      round = doRound message (Ballot myAddr crs myData) members
      act = runRound round waiter
  hathReader (getProc . has) act


majoritySigs :: Int -> RoundWaiter a [Ballot a]
majoritySigs requiredSigs ballots = do
  if length ballots >= requiredSigs
     then Just ballots
     else Nothing


-- 2 round agreement process
-- round 1 is collect sigs for the tx
-- round 2 is who will send it
agreeTx :: (Has Mandate r, Has GethConfig r) => Msg -> Hath r (Maybe ([CompactRecSig], Address))
agreeTx toSign = do
  (requiredSigs, members) <- mandateGetMembers
  
  let round1 = do
        runConsensusRound toSign () $ majoritySigs requiredSigs

  let round2 results = do
        let membersWhoSigned = bMember <$> results
            (candidate, _) = head $ sortOn (sha3' . abi "" ) [(bMember r, getMsg toSign) | r <- results]
            round2topic = toMsg $ abi "round2" (getMsg toSign)
        runConsensusRound round2topic candidate $ majoritySigs requiredSigs

  runMaybeT $ do
    results1 <- MaybeT round1
    threadDelay 10000000
    results2 <- MaybeT $ round2 results1
    let sigs = bSig <$> results1
    address <- MaybeT $ pure $ countVotes results2 $ length members
    pure $ (sigs, address)


countVotes :: (Ord a, Show a) => [Ballot a] -> Int -> Maybe a
countVotes ballots total =
  let objs = bData <$> ballots
      votes = Map.fromListWith (+) $ zip objs $ repeat 1
      (obj, n) = head $ sortOn (\c -> snd c * (-1)) $ Map.toList votes
   in if n < quot total 2 + 1
         then Nothing
         else Just obj
