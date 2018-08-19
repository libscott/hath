{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Hath.Mandate.Round
  ( AgreementProcess(..)
  , Ballot(..)
  , agreeCollectSigs
  , spawnAgree
  , inventoryIndex
  , prioritiseRemoteInventory
  , dedupeInventoryQueries
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
import           Data.Typeable

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


agreeCollectSigs :: (Ser.Serialize a, Typeable a) => Msg -> Ballot a -> [Address] ->
                    Hath AgreementProcess [Ballot a]
agreeCollectSigs message myBallot members = do
  let topic = asString $ getMsg message
      (Ballot myAddress mySig myData) = myBallot

  startTime <- liftIO $ getCurrentTime
  let timeoutMs = 30000000 -- 30s
      sendMine = P2P.nsendPeers topic (mySig, SerBinary myData)

  let f sigs | Map.size sigs == length members = pure sigs
      f sigs = do
        t <- diffUTCTime startTime <$> liftIO getCurrentTime
        let ms = round $ (realToFrac t) * 1000000 + timeoutMs
        say $ "μs: " ++ show ms
        mTheirSig <- expectTimeout $ max ms 0
        
        case mTheirSig of
             Nothing -> pure sigs -- Timeout
             Just (theirSig, SerBinary obj) ->
               case recoverAddr message theirSig of
                    Just addr ->
                      if elem addr members
                         then do
                           say $ "Got sig from peer: " ++ show addr
                           when (not $ Map.member addr sigs) sendMine
                           f $ Map.insert addr (Ballot addr theirSig obj) sigs
                         else do
                           say $ "Not member: " ++ show addr
                           f sigs
                    Nothing -> do
                      say "Signature recovery failed"
                      f sigs

  runAgree $ do
    getSelfPid >>= register topic
    sendMine
    out <- f $ Map.singleton myAddress myBallot
    unregister topic
    pure $ snd <$> Map.toList out


newtype SerBinary a = SerBinary { unSerBinary :: a }
  deriving (Typeable)

instance Ser.Serialize a => Binary (SerBinary a) where
  put = put . Ser.encode . unSerBinary
  get = do
    bs <- get
    either fail (pure . SerBinary) $ Ser.decode bs


type Inventory a = Map Address (CompactRecSig, a)

data RoundMsg a =
    InventoryIndex ProcessId Integer
  | GetInventory ProcessId Integer
  | InventoryData (Inventory a)
  deriving (Generic)

instance Binary a => Binary (RoundMsg a)

data Round a = Round
  { topic :: String
  , mInv :: MVar (Inventory a)
  , members :: [Address]
  , parent :: ProcessId
  }

repeatExpect :: Serializable a => Int -> (a -> Process ()) -> Process ()
repeatExpect usTimeout act = do
  startTime <- liftIO $ getCurrentTime
  fix $ \f -> do
    t <- diffUTCTime startTime <$> liftIO getCurrentTime
    let us = max 0 $ round $ (realToFrac t) * 1000000 + fromIntegral usTimeout
    when (us > 0) $
       expectTimeout us >>= maybe (pure ()) (\a -> act a >> f)

doRound :: forall a. Serializable a => Msg -> Ballot a -> [Address] -> Process [Ballot a]
doRound message myBallot members = do
  let topic = asString $ getMsg message
      (Ballot myAddr mySig myObj) = myBallot

  let startInventory = Map.fromList [(myAddr, (mySig, myObj))]
  mInv <- liftIO $ newMVar startInventory
  myPid <- getSelfPid
  let round = Round topic mInv members myPid

  invBuilder <- spawnLocal $ do
    link myPid >> buildInventory round

  let handleMsg addr theirSig =
        \case
          (InventoryIndex peer idx) -> do
            send invBuilder (peer, idx)
          (GetInventory peer idx) -> do
            inv <- liftIO $ readMVar mInv
            let idx = inventoryIndex members inv
            send peer (InventoryData $ getInventorySubset idx members inv :: RoundMsg a)
          (InventoryData inv) -> do
            -- TODO: authenticate
            liftIO $ modifyMVar_ mInv $ pure . Map.union inv

  
  register topic myPid
  P2P.nsendPeers topic $ InventoryData startInventory

  repeatExpect (10 * 1000000) $
    \(theirSig, obj) -> do
      case recoverAddr message theirSig of
           Just addr ->
             if elem addr members
                then do
                  say $ "Got sig from peer: " ++ show addr
                  handleMsg addr theirSig (obj :: RoundMsg a)
                else do
                  say $ "Not member or wrong round: " ++ show addr
           Nothing -> do
             say "Signature recovery failed"

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
      send peer (GetInventory parent wanted :: RoundMsg a)

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
