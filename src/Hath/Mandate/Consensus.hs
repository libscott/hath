{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Hath.Mandate.Consensus where

import           Control.Distributed.Process.Serializable (Serializable)

import qualified Data.Map as Map

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC

import           Hath.Prelude
import           Hath.Mandate.Round
import           Hath.Mandate.Types
import Debug.Trace


runRound :: (Has GethConfig r, Has Mandate r, Serializable a, Show a) => Msg -> a -> Hath r [Ballot a]
runRound message myData = do
  logInfo $ "Round topic: " ++ show (toHex $ getMsg message)
  (sk, myAddr) <- asks $ getMe . has
  (_, members) <- mandateGetMembers
  let crs = sign sk message
      act = runAgree $ doRound message (Ballot myAddr crs myData) members
  hathReader (getProc . has) act


-- 2 round agreement process
-- round 1 is collect sigs for the tx
-- round 2 is who will send it
agreeTx :: (Has Mandate r, Has GethConfig r) => Msg -> Hath r (Maybe ([CompactRecSig], Address))
agreeTx toSign = do
  (requiredSigs, members) <- mandateGetMembers
  
  let round1 = do
        results <- runRound toSign ()
        pure $ if length results >= requiredSigs then Just results else Nothing

  let round2 results = do
        let membersWhoSigned = bMember <$> results
            (candidate, _) = head $ sortOn (sha3' . abi "" ) [(bMember r, getMsg toSign) | r <- results]
            round2topic = toMsg $ abi "round2" (getMsg toSign)
        runRound round2topic candidate

  round1 >>=
    \case
      Nothing -> pure Nothing
      Just results1 -> do
            results2 <- round2 results1
            if length results2 >= requiredSigs
               then do
                 let sigs = bSig <$> results1
                 pure $ ((,) sigs) <$> countVotes results2 (length members)
               else pure Nothing


countVotes :: (Ord a, Show a) => [Ballot a] -> Int -> Maybe a
countVotes ballots total =
  let objs = bData <$> ballots
      votes = Map.fromListWith (+) $ zip objs $ repeat 1
      (obj, n) = head $ sortOn (\c -> snd c * (-1)) $ Map.toList votes
   in if n < quot total 2 + 1
         then Nothing
         else Just obj
