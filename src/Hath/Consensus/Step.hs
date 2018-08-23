{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Hath.Consensus.Step
  ( Ballot(..)
  , StepMsg(..)
  , Inventory
  , inventoryIndex
  , prioritiseRemoteInventory
  , dedupeInventoryQueries
  , runStep
  , runSeed
  ) where

import           Control.Monad

import           Control.Distributed.Process as DP
import           Control.Distributed.Process.Node as DPN
import           Control.Distributed.Process.Serializable (Serializable)

import           Data.Binary
import           Data.Bits
import qualified Data.Serialize as Ser
import qualified Data.Map as Map
import           Data.Time.Clock

import           GHC.Generics (Generic)

import           Network.Ethereum.Crypto
import qualified Hath.Consensus.P2P as P2P
import           Hath.Lifted
import           Hath.Prelude
import Debug.Trace


runSeed :: IO ()
runSeed = do
  let host = "localhost"
      port = "18089"
      ext = const (host, port)
  node <- P2P.createLocalNode host port ext initRemoteTable
  runProcess node $ P2P.peerController []

data Ballot a = Ballot
  { bMember :: Address
  , bSig :: CompactRecSig
  , bData :: a
  } deriving (Show, Generic)

instance Binary a => Binary (Ballot a)

type Inventory a = Map Address (CompactRecSig, a)

data StepMsg a =
    JoinStep ProcessId (Inventory a)
  | InventoryIndex ProcessId Integer
  | GetInventory ProcessId Integer
  | InventoryData (Inventory a)
  deriving (Show, Generic)

instance Binary a => Binary (StepMsg a)

data Step a = Step
  { topic :: String
  , mInv :: MVar (Inventory a)
  , members :: [Address]
  , parent :: ProcessId
  , mySend :: (ProcessId -> StepMsg a -> Process ())
  }

runStep :: forall a. Serializable a
        => Msg -> Ballot a -> [Address] -> (Inventory a -> Process ()) -> Process ()
runStep message myBallot members yield = do
  let topic = asString $ getMsg message
      (Ballot myAddr mySig myData) = myBallot

  mInv <- newMVar Map.empty
  myPid <- getSelfPid
  let mySend peer a = send peer ((mySig, a) :: (CompactRecSig, StepMsg a))
  let step = Step topic mInv members myPid mySend

  invBuilder <- spawnLocal $ do
    link myPid >> buildInventory step

  let handleMsg =
        \case
          (InventoryIndex peer theirIdx) -> do
            send invBuilder (peer, theirIdx)
          (GetInventory peer idx) -> do
            inv <- readMVar mInv
            let idx = inventoryIndex members inv
            let subset = getInventorySubset idx members inv
            mySend peer $ InventoryData subset
          (InventoryData theirInv) -> do
            -- TODO: authenticate (authmax)
            modifyMVar_ mInv $ pure . Map.union theirInv

            inv <- readMVar mInv
            yield inv

            -- TODO: Make sure we are adding something new before
            -- broadcasting
            let idx = inventoryIndex members inv
            P2P.nsendPeers topic (mySig, (InventoryIndex myPid idx :: StepMsg a))
            say $ "My inventory: " ++ show idx


  register topic myPid

  let onStepMsg (theirSig, obj) = do
        case recoverAddr message theirSig of
             Just addr ->
               if elem addr members
                  then do
                    handleMsg (obj :: StepMsg a)
                  else do
                    say $ "Not member or wrong step: " ++ show addr
             Nothing -> do
               say "Signature recovery failed"

  let onNewPeer (P2P.NewPeer nodeId) = do
        inv <- readMVar mInv
        let idx = inventoryIndex members inv
        nsendRemote nodeId topic (mySig, (InventoryIndex myPid idx :: StepMsg a))

  repeatMatch (5000 * 1000000) [match onStepMsg, match onNewPeer]
  say "main ended"

repeatMatch :: Int -> [Match ()] -> Process ()
repeatMatch usTimeout matches = do
  startTime <- liftIO getCurrentTime
  fix $ \f -> do
    t <- diffUTCTime startTime <$> liftIO getCurrentTime
    let us = max 0 $ round $ (realToFrac t) * 1000000 + fromIntegral usTimeout
    when (us > 0) $
       receiveTimeout us matches >>= maybe (pure ()) (\() -> f)

buildInventory :: forall a. Serializable a => Step a -> Process ()
buildInventory step@Step{..} = do
  say "inventory"
  forever $ do
    -- Stage 1: Take a few remote indexes and send requests
    idxs <- recvAll :: Process [(ProcessId, Integer)]
    ordered <- prioritiseRemoteInventory members <$> readMVar mInv <*> pure idxs
    let queries = dedupeInventoryQueries ordered
    forM_ queries $ \(peer, wanted) -> do
      mySend peer $ GetInventory parent wanted
    -- Stage 2: Take a nap
    threadDelay $ 500 * 1000

-- | Get a bit array of what inventory we have
inventoryIndex :: [Address] -> Map Address a -> Integer
inventoryIndex members inv =
  let have addr = if Map.member addr inv then 1 else 0
      shiftHave n (i, addr) = n .|. shift (have addr) i
   in foldl shiftHave 0 $ zip [0..] members

-- | Sort remote inventories by how interesting they are and remote inventory we already have
prioritiseRemoteInventory :: [Address] -> Map Address a -> [(b, Integer)] -> [(b, Integer)]
prioritiseRemoteInventory members inv idxs =
  let myIdx = inventoryIndex members inv
      interesting = [(p, i .&. complement myIdx) | (p, i) <- idxs]
   in sortOn (\(pid,i) -> popCount i * (-1)) interesting

-- Get queries for remote inventory filtering duplicates
dedupeInventoryQueries :: [(a, Integer)] -> [(a, Integer)]
dedupeInventoryQueries = f 0
  where f seen ((peer, idx):xs) =
          let wanted = idx .&. complement seen
              rest = f (seen .|. wanted) xs
           in if wanted /= 0
                 then (peer, wanted) : rest
                 else []
        f _ [] = []

-- Get part of the inventory according to an index
getInventorySubset :: Integer -> [Address] -> Inventory a -> Inventory a
getInventorySubset idx members =
  Map.filterWithKey $
    \k _ -> let Just i = elemIndex k members
             in testBit idx i

recvAll :: Serializable a => Process [a]
recvAll = expectTimeout 0 >>= maybe (pure []) (\a -> (a:) <$> recvAll)
