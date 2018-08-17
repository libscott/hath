
module Hath.Mandate.Agree where

import           Control.Monad
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar

import qualified Control.Distributed.Backend.P2P as P2P
import           Control.Distributed.Process as DP
import           Control.Distributed.Process.Node as DPN

import qualified Data.Map as Map
import           Data.Time.Clock

import           Network.Ethereum.Crypto
import           Hath.Prelude


runSeed :: IO ()
runSeed = do
  let host = "localhost"
      port = "18089"
      ext = const (host, port)
  node <- P2P.createLocalNode host port ext initRemoteTable
  runProcess node $ P2P.peerController []

runAgree :: Process a -> IO a
runAgree act = do
  let host = "localhost"
      port = "18088"
      ext = const (host, port)
      seeds = [P2P.makeNodeId "localhost:18089"]

  outVar <- newEmptyMVar

  node <- P2P.createLocalNode host port ext initRemoteTable
  _ <- forkProcess node $ P2P.peerController seeds
  runProcess node $ P2P.waitController $
    act >>= liftIO . putMVar outVar
  closeLocalNode node
  takeMVar outVar


agreeCollectSigs :: Msg -> CompactRecSig -> Int -> [Address] ->
                    IO (Bool, [(Address, CompactRecSig)])
agreeCollectSigs message sig sigsRequired members = do
  let topic = asString $ getMsg message
      Just myAddress = recoverAddr message sig

  startTime <- getCurrentTime

  let f sigs | Map.size sigs >= sigsRequired = pure sigs
      f sigs = do
        t <- diffUTCTime <$> liftIO getCurrentTime <*> pure startTime
        let ms = round $ realToFrac t * 1000
        mTheirSig <- expectTimeout ms :: Process (Maybe CompactRecSig)

        case mTheirSig of
             Just theirSig ->
               case recoverAddr message theirSig of
                    Just addr ->
                      if elem addr members
                         then f $ Map.insert addr theirSig sigs
                         else do
                           say $ "Not member: " ++ show addr
                           f sigs
                    Nothing -> do
                      say "Signature recovery failed"
                      f sigs
             Nothing -> pure sigs -- Timeout

  runAgree $ do
    getSelfPid >>= register topic
    P2P.nsendPeers topic sig
    out <- f $ Map.singleton myAddress sig
    unregister topic
    liftIO $ threadDelay 1000000
    pure (length out >= sigsRequired, Map.toList out)

