{-# LANGUAGE FlexibleContexts #-}

module Hath.Mandate.Agree where

import           Control.Monad
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar

import           Control.Distributed.Process as DP
import           Control.Distributed.Process.Node as DPN

import           Data.Binary
import qualified Data.Serialize as Ser
import qualified Data.Map as Map
import           Data.Time.Clock
import           Data.Typeable

import           Network.Ethereum.Crypto
import qualified Hath.Mandate.P2P as P2P
import           Hath.Prelude


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
    liftIO $ threadDelay 500000
    getSelfPid >>= register topic
    liftIO $ threadDelay 500000
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
