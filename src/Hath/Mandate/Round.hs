{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Hath.Mandate.Round
  ( AgreementProcess(..)
  , Ballot(..)
  , spawnAgree
  , inventoryIndex
  , prioritiseRemoteInventory
  , dedupeInventoryQueries
  , runAgree
  , doRound
  , runSeed
  ) where

import           Control.Monad
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar

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
import qualified Hath.Mandate.P2P as P2P
import           Hath.Prelude
import Debug.Trace


runSeed :: IO ()
runSeed = do
  let host = "localhost"
      port = "18089"
      ext = const (host, port)
  node <- P2P.createLocalNode host port ext initRemoteTable
  runProcess node $ P2P.peerController []


data AgreementProcess = AgreementProcess
  { apNode :: LocalNode
  }

spawnAgree :: String -> String -> IO AgreementProcess
spawnAgree seed port = do
  let host = "localhost"
      ext = const (host, port)
      seeds = [P2P.makeNodeId seed]
  (node, _) <- P2P.startP2P host port ext initRemoteTable seeds
  pure $ AgreementProcess node


runAgree :: Has AgreementProcess r => Process a -> Hath r a
runAgree act = do
  node <- asks $ apNode . has
  liftIO $ do
    handoff <- newEmptyMVar
    runProcess node $ act >>= liftIO . putMVar handoff
    takeMVar handoff

data Ballot a = Ballot
  { bMember :: Address
  , bSig :: CompactRecSig
  , bData :: a
  } deriving (Show)

type Inventory a = Map Address (CompactRecSig, a)

data RoundMsg a =
    JoinRound ProcessId (Inventory a)
  | InventoryIndex ProcessId Integer
  | GetInventory ProcessId Integer
  | InventoryData (Inventory a)
  deriving (Show, Generic)

instance Binary a => Binary (RoundMsg a)

data Round a = Round
  { topic :: String
  , mInv :: MVar (Inventory a)
  , members :: [Address]
  , parent :: ProcessId
  , mySend :: (ProcessId -> RoundMsg a -> Process ())
  }

repeatMatch :: Int -> [Match ()] -> Process ()
repeatMatch usTimeout matches = do
  startTime <- liftIO $ getCurrentTime
  fix $ \f -> do
    t <- diffUTCTime startTime <$> liftIO getCurrentTime
    let us = max 0 $ round $ (realToFrac t) * 1000000 + fromIntegral usTimeout
    when (us > 0) $
       receiveTimeout us matches >>= maybe (pure ()) (\() -> f)

doRound :: forall a. (Serializable a, Show a) => Msg -> Ballot a -> [Address] -> Process [Ballot a]
doRound message myBallot members = do
  let topic = asString $ getMsg message
      (Ballot myAddr mySig myObj) = myBallot

  let startInventory = Map.fromList [(myAddr, (mySig, myObj))]
  mInv <- liftIO $ newMVar startInventory
  myPid <- getSelfPid
  let mySend peer a = send peer ((mySig, a) :: (CompactRecSig, RoundMsg a))
  let round = Round topic mInv members myPid mySend

  invBuilder <- spawnLocal $ do
    link myPid >> buildInventory round

  let handleMsg addr =
        \case
          (InventoryIndex peer theirIdx) -> do
            send invBuilder (peer, theirIdx)
          (GetInventory peer idx) -> do
            inv <- liftIO $ readMVar mInv
            let idx = inventoryIndex members inv
            let subset = getInventorySubset idx members inv
            mySend peer $ InventoryData subset
          (InventoryData inv) -> do
            -- TODO: authenticate
            liftIO $ modifyMVar_ mInv $ pure . Map.union inv
            
            -- TODO: Make sure we are adding something new before
            -- broadcasting
            inv <- liftIO $ readMVar mInv
            let idx = inventoryIndex members inv
            P2P.nsendPeers topic (mySig, (InventoryIndex myPid idx :: RoundMsg a))
            say $ "My inventory: " ++ show idx

  
  -- The only thing we use the p2p library for is to bootstrap
  -- the peer list and send a message to all peers based on a topic.
  -- Which is already quite useful really.
  register P2P.peerListenerService myPid
  register topic myPid
  P2P.nsendPeers topic (mySig, InventoryData startInventory)

  let onRoundMsg (theirSig, obj) = do
        case recoverAddr message theirSig of
             Just addr ->
               if elem addr members
                  then do
                    handleMsg addr (obj :: RoundMsg a)
                  else do
                    say $ "Not member or wrong round: " ++ show addr
             Nothing -> do
               say "Signature recovery failed"

  let onNewPeer (P2P.NewPeer nodeId) = do
        inv <- liftIO $ readMVar mInv
        let idx = inventoryIndex members inv
        nsendRemote nodeId topic (mySig, (InventoryIndex myPid idx :: RoundMsg a))

  repeatMatch (5 * 1000000) [match onRoundMsg, match onNewPeer]

  r <- Map.toAscList <$> liftIO (takeMVar mInv)
  pure $ [Ballot a s o | (a, (s, o)) <- r]

buildInventory :: forall a. Serializable a => Round a -> Process ()
buildInventory round@Round{..} =
  forever $ do
    -- Stage 1: Take a few remote indexes and send requests
    idxs <- recvAll :: Process [(ProcessId, Integer)]
    ordered <- prioritiseRemoteInventory members <$> liftIO (readMVar mInv) <*> pure idxs
    let queries = dedupeInventoryQueries ordered
    forM_ queries $ \(peer, wanted) -> do
      mySend peer $ GetInventory parent wanted

    -- Stage 2: Take a nap
    liftIO $ threadDelay $ 500 * 1000

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
