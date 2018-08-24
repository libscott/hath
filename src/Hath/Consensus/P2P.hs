{-# LANGUAGE OverloadedStrings, RecordWildCards, MonoLocalBinds, DeriveGeneric #-}

-- Kindly borrowed from: http://hackage.haskell.org/package/distributed-process-p2p
--
--
-- | Peer-to-peer node discovery backend for Cloud Haskell based on the TCP
-- transport. Provided with a known node address it discovers and maintains
-- the knowledge of it's peers.
--
-- > import qualified Control.Distributed.Backend.P2P as P2P
-- > import           Control.Monad.Trans (liftIO)
-- > import           Control.Concurrent (threadDelay)
-- >
-- > main = P2P.bootstrap "myhostname" "9001" [P2P.makeNodeId "seedhost:9000"] $ do
-- >     liftIO $ threadDelay 1000000 -- give dispatcher a second to discover other nodes
-- >     P2P.nsendPeers "myService" ("some", "message")

module Hath.Consensus.P2P (
    -- * Starting peer controller
    NewPeer(..),
    startP2P,
    makeNodeId,
    getPeers,
    getCapable,
    nsendPeers,
    nsendCapable,
    createLocalNode,
    peerController,
    peerListenerService,
    runSeed
) where

import Control.Distributed.Process                as DP
import Control.Distributed.Process.Node           as DPN
import Control.Distributed.Process.Serializable (Serializable)
import Network.Transport (EndPointAddress(..))
import Network.Socket (HostName, ServiceName)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import Data.Maybe (isJust)

import Data.Binary
import Data.Typeable

import GHC.Generics (Generic)

-- * Peer-to-peer API

type Peers = S.Set ProcessId

data PeerState = PeerState { p2pPeers :: MVar Peers }

initPeerState :: Process PeerState
initPeerState = do
    self <- getSelfPid
    peers <- liftIO $ newMVar (S.singleton self)
    return $! PeerState peers

runSeed :: String -> String -> IO ()
runSeed host port = do
  let ext = const (host, port)
  node <- createLocalNode host port ext initRemoteTable
  runProcess node $ peerController []

-- ** Initialization

-- | Make a NodeId from "host:port" string.
makeNodeId :: String -> NodeId
makeNodeId addr = NodeId . EndPointAddress . BS.concat $ [BS.pack addr, ":0"]

-- | Like 'bootstrap' but use 'forkProcess' instead of 'runProcess'. Returns local node and pid of given process
startP2P
  :: HostName
  -> ServiceName
  -> (ServiceName -> (HostName, ServiceName))
  -> RemoteTable
  -> [NodeId]
  -> IO (LocalNode, ProcessId)
startP2P host port ext rTable seeds = do
    node <- createLocalNode host port ext rTable
    pid <- forkProcess node $ peerController seeds
    runProcess node $ waitController $ pure ()
    return (node, pid)

-- | Waits for controller to start, then runs given process
waitController :: Process a -> Process a
waitController prc = do
    res <- whereis peerControllerService
    case res of
        Nothing -> (liftIO $ threadDelay 100000) >> waitController prc
        Just _ -> maySay "Bootstrap complete." >> prc

-- | Creates tcp local node which used by 'bootstrap'
createLocalNode
  :: HostName
  -> ServiceName
  -> (ServiceName -> (HostName, ServiceName))
  -> RemoteTable
  -> IO LocalNode
createLocalNode host port mkExternal rTable = do
    transport <- either (error . show) id
                 <$> createTransport host port mkExternal defaultTCPParameters
    newLocalNode transport rTable

peerControllerService :: String
peerControllerService = "P2P:Controller"

peerListenerService :: String
peerListenerService = "P2P::Listener"

-- | A P2P controller service process.
peerController :: [NodeId] -> Process ()
peerController seeds = do
    state <- initPeerState

    getSelfPid >>= register peerControllerService

    mapM_ doDiscover seeds
    maySay "P2P controller started."
    forever $ receiveWait [ matchIf isPeerDiscover $ onDiscover state
                          , match $ onMonitor state
                          , match $ onPeerRequest state
                          , match $ onPeerResponse state
                          , match $ onPeerQuery state
                          , match $ onPeerCapable
                          ]

-- ** Discovery

-- Goes from a node to a process
doDiscover :: NodeId -> Process ()
doDiscover node = do
    maySay $ "Examining node: " ++ show node
    whereisRemoteAsync node peerControllerService

data NewPeer = NewPeer NodeId
  deriving (Generic)

instance Binary NewPeer

doRegister :: PeerState -> ProcessId -> Process ()
doRegister (PeerState{..}) pid = do
    pids <- liftIO $ takeMVar p2pPeers
    if S.member pid pids
        then liftIO $ putMVar p2pPeers pids
        else do
            maySay $ "Registering peer:" ++ show pid
            _ <- monitor pid

            liftIO $ putMVar p2pPeers (S.insert pid pids)
            maySay $ "New node: " ++ show pid
            doDiscover $ processNodeId pid
            nsend peerListenerService $ NewPeer $ processNodeId pid

doUnregister :: PeerState -> Maybe MonitorRef -> ProcessId -> Process ()
doUnregister PeerState{..} mref pid = do
    maySay $ "Unregistering peer: " ++ show pid
    maybe (return ()) unmonitor mref
    peers <- liftIO $ takeMVar p2pPeers
    liftIO $ putMVar p2pPeers (S.delete pid peers)

isPeerDiscover :: WhereIsReply -> Bool
isPeerDiscover (WhereIsReply service pid) =
    service == peerControllerService && isJust pid

data GiveMePeers = GiveMePeers
  deriving (Typeable)

instance Binary GiveMePeers where
  put GiveMePeers = pure ()
  get = pure GiveMePeers

onDiscover :: PeerState -> WhereIsReply -> Process ()
onDiscover _     (WhereIsReply _ Nothing) = return ()
onDiscover state (WhereIsReply _ (Just seedPid)) = do
    say $ "Peer discovered: " ++ show seedPid
    self <- getSelfPid
    send seedPid (self, GiveMePeers)

onPeerResponse :: PeerState -> (ProcessId, Peers) -> Process ()
onPeerResponse state (peer, peers) = do
    maySay $ "Got peers from: " ++ show peer
    known <- liftIO $ readMVar $ p2pPeers state
    mapM_ (doRegister state) (S.toList $ S.difference peers known)

onPeerRequest :: PeerState -> (ProcessId, GiveMePeers) -> Process ()
onPeerRequest PeerState{..} (peer, _) = do
    maySay $ "Peer exchange with " ++ show peer
    peers <- liftIO $ takeMVar p2pPeers
    if S.member peer peers
        then liftIO $ putMVar p2pPeers peers
        else do
            _ <- monitor peer
            liftIO $ putMVar p2pPeers (S.insert peer peers)

    self <- getSelfPid
    send peer (self, peers)

onPeerQuery :: PeerState -> SendPort Peers -> Process ()
onPeerQuery PeerState{..} replyTo = do
    maySay $ "Local peer query."
    liftIO (readMVar p2pPeers) >>= sendChan replyTo

onPeerCapable :: (String, SendPort ProcessId) -> Process ()
onPeerCapable (service, replyTo) = do
    maySay $ "Capability request: " ++ service
    res <- whereis service
    case res of
        Nothing -> maySay "I can't."
        Just pid -> maySay "I can!" >> sendChan replyTo pid

onMonitor :: PeerState -> ProcessMonitorNotification -> Process ()
onMonitor state (ProcessMonitorNotification mref pid reason) = do
    maySay $ "Monitor event: " ++ show (pid, reason)
    doUnregister state (Just mref) pid

-- ** Discovery

-- | Get a list of currently available peer nodes.
getPeers :: Process [NodeId]
getPeers = do
    maySay $ "Requesting peer list from local controller..."
    (sp, rp) <- newChan
    nsend peerControllerService (sp :: SendPort Peers)
    receiveChan rp >>= return . map processNodeId . S.toList

-- | Poll a network for a list of specific service providers.
getCapable :: String -> Process [ProcessId]
getCapable service = do
    (sp, rp) <- newChan
    nsendPeers peerControllerService (service, sp)
    maySay "Waiting for capable nodes..."
    go rp []

    where go rp acc = do res <- receiveChanTimeout 100000 rp
                         case res of Just pid -> maySay "cap hit" >> go rp (pid:acc)
                                     Nothing -> maySay "cap done" >> return acc

-- ** Messaging

-- | Broadcast a message to a specific service on all peers.
nsendPeers :: Serializable a => String -> a -> Process ()
nsendPeers service msg = getPeers >>= mapM_ (\peer -> nsendRemote peer service msg)

-- | Broadcast a message to a service of on nodes currently running it.
nsendCapable :: Serializable a => String -> a -> Process ()
nsendCapable service msg = getCapable service >>= mapM_ (\pid -> send pid msg)

maySay :: String -> Process ()
maySay _ = pure ()
