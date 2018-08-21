{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Hath.Mandate.Consensus where

import           Control.Monad

import           Control.Distributed.Process as CDP
import           Control.Distributed.Process.Serializable (Serializable)
import           Control.Monad.Trans.Maybe

import           Data.Binary
import qualified Data.Map as Map

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC

import           GHC.Generics (Generic)

import           Hath.Prelude
import           Hath.Lifted
import qualified Hath.Mandate.P2P as P2P
import           Hath.Mandate.Round
import           Hath.Mandate.Types
import Debug.Trace


permute :: Msg -> Msg
permute = toMsg . getMsg

data From = From1 [Ballot ()] | From2 [Ballot Address]
  deriving (Generic)

instance Binary From

agreeMsgFast :: (Has Mandate r, Has GethConfig r)
             => Msg -> Hath r ([CompactRecSig], Address)
agreeMsgFast toSign = do
  (sk, myAddr) <- asks $ getMe . has
  (requiredSigs, members) <- mandateGetMembers

  hathReader (getProc . has) $ do
    runProcGet $ do
      top <- getSelfPid

      -- round 1
      let msg1 = toSign
          sig1 = sign sk msg1
          ballot1 = Ballot myAddr sig1 ()
      pid1 <- spawnLocal $ doRound msg1 ballot1 members (send top . From1)

      -- round 2
      let msg2 = permute toSign
          sig2 = sign sk msg2
          ballot2 = Ballot myAddr sig2 (undefined::Address)
      pid2 <- spawnLocal $ doRound msg2 ballot2 members (send top . From2)

      -- peernotifier
      _ <- spawnLocal $ do
        getSelfPid >>= register P2P.peerListenerService
        forever $ do
          m <- expect :: Process P2P.NewPeer
          send pid1 m >> send pid2 m

      let run votes (n2,v2) = do

            let from1 ballots = do
                  if length ballots >= requiredSigs
                     then do
                       let votes' = bSig <$> ballots
                       let obj = determineSender ballots toSign
                       if n2 == 0 || obj /= v2
                          then do
                            let newData = (n2+1, obj) :: (Int, Address)
                            send pid2 (sig2, InventoryData $ Map.singleton myAddr (sig2, newData) :: RoundMsg Address)
                            run votes' newData
                          else run votes' (n2, v2)
                  else run votes (n2, v2)

            let from2 ballots = do
                  if (length ballots >= requiredSigs)
                     then do
                       case countVotes ballots (length members) of
                         Nothing -> run votes (n2, v2)
                         Just r -> pure (votes, r)
                     else run votes (n2, v2)

            r <- expect
            case r of From1 b -> from1 b
                      From2 b -> from2 b

      send pid1 (sig1, InventoryData $ Map.singleton myAddr (sig1, (0, ())) :: RoundMsg ())
      run [] (0, undefined)



determineSender :: [Ballot a] -> Msg -> Address
determineSender ballots message =
  fst $ head $ sortOn (sha3' . abi "" ) [(bMember r, getMsg message) | r <- ballots]


countVotes :: (Ord a, Show a) => [Ballot a] -> Int -> Maybe a
countVotes ballots total =
  let objs = bData <$> ballots
      votes = Map.fromListWith (+) $ zip objs $ repeat 1
      (obj, n) = head $ sortOn (\c -> snd c * (-1)) $ Map.toList votes
   in if n < quot total 2 + 1
         then Nothing
         else Just obj
