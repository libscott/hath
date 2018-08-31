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
    peerController,
    peerListenerService,
    runSeed,
    peerNotifier
) where

import Control.Distributed.Process                as DP
import Control.Distributed.Process.Node           as DPN
import Control.Distributed.Process.Serializable (Serializable)
import Network.Transport (EndPointAddress(..))
import Network.Socket (HostName, ServiceName)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import Data.Maybe (isJust)
import Data.Binary
import Data.Typeable

import Hath.Prelude
import Hath.Prelude.Lifted
import Hath.Consensus.Utils

import GHC.Generics (Generic)
import System.Posix.Signals

-- * Peer-to-peer API

peerControllerService :: String
peerControllerService = "P2P:Controller"


type Peers = S.Set ProcessId

data PeerState = PeerState { p2pPeers :: MVar Peers }

initPeerState :: Process PeerState
initPeerState = do
    self <- getSelfPid
    peers <- newMVar (S.singleton self)
    return $! PeerState peers

runSeed :: String -> Word16 -> IO ()
runSeed host port = do
  let ext = const (host, show port)
  node <- createLocalNode host (show port) ext initRemoteTable
  runProcess node $ peerController []

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


-- ** Initialization

-- | Make a NodeId from "host:port" string.
makeNodeId :: String -> NodeId
makeNodeId addr = NodeId . EndPointAddress . BS.concat $ [BS.pack addr, ":0"]

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
waitController act = do
  fix $ \f -> do
    res <- whereis peerControllerService
    case res of
        Nothing -> threadDelay 100000 >> f
        Just _ -> act

dumpPeers :: PeerState -> IO ()
dumpPeers (PeerState mpeers) = do
  peers <- S.toAscList <$> readMVar mpeers
  runHath () $ do
    logInfo "Got signal USR1"
    forM_ peers $ \p ->
      logInfo $ show p

-- | A P2P controller service process.
peerController :: [NodeId] -> Process ()
peerController seeds = do
    state <- initPeerState
    liftIO $ do
      installHandler sigUSR1 (Catch $ dumpPeers state) Nothing
    getSelfPid >>= register peerControllerService

    forever $ do
      mapM_ doDiscover seeds
      repeatMatch 60000000 [ matchIf isPeerDiscover $ onDiscover state
                           , match $ onMonitor state
                           , match $ onPeerHello state
                           , match $ onPeerResponse state
                           , match $ onPeerQuery state
                           , match $ onPeerCapable
                           ]
-- ** Discovery

-- 0: A node probes another node
doDiscover :: NodeId -> Process ()
doDiscover node = do
    maySay $ "Examining node: " ++ show node
    whereisRemoteAsync node peerControllerService

-- 1: Discovery reply, other node's peercontrollerservice
isPeerDiscover :: WhereIsReply -> Bool
isPeerDiscover (WhereIsReply service pid) =
    service == peerControllerService && isJust pid

-- 1.1: Register that peer and ask for their peers
onDiscover :: PeerState -> WhereIsReply -> Process ()
onDiscover _     (WhereIsReply _ Nothing) = return ()
onDiscover state (WhereIsReply _ (Just pid)) =
  newPeer state pid

-- 2: When there's a request to share peers
onPeerHello :: PeerState -> (ProcessId, Hello) -> Process ()
onPeerHello s@PeerState{..} (peer, Hello) = do
    maySay $ "Peer exchange with " ++ show peer
    self <- getSelfPid
    peers <- readMVar p2pPeers
    send peer (self, peers)
    newPeer s peer

-- 3: When peers are received
onPeerResponse :: PeerState -> (ProcessId, Peers) -> Process ()
onPeerResponse state (peer, peers) = do
    maySay $ "Got peers from: " ++ show peer
    known <- readMVar $ p2pPeers state
    -- Do a discovery here so when we get a response we know the node is up
    mapM_ (doDiscover . processNodeId) $ S.toList $ S.difference peers known

-- 4: Disconnect
onMonitor :: PeerState -> ProcessMonitorNotification -> Process ()
onMonitor PeerState{..} (ProcessMonitorNotification mref pid reason) = do
    say $ "Unregister peer: " ++ show (pid, reason)
    maybe (return ()) unmonitor $ Just mref
    modifyMVar_ p2pPeers $ pure . S.delete pid

newPeer :: PeerState -> ProcessId -> Process ()
newPeer (PeerState{..}) pid = do
  pids <- liftIO $ takeMVar p2pPeers
  if S.member pid pids
     then putMVar p2pPeers pids
     else do
       say $ "New peer:" ++ show pid
       putMVar p2pPeers $ S.insert pid pids
       _ <- monitor pid
       nsend peerListenerService $ NewPeer $ processNodeId pid
       self <- getSelfPid
       send pid (self, Hello)




data Hello = Hello
  deriving (Typeable, Generic)

instance Binary Hello

onPeerQuery :: PeerState -> SendPort Peers -> Process ()
onPeerQuery PeerState{..} replyTo = do
    maySay $ "Local peer query."
    readMVar p2pPeers >>= sendChan replyTo

onPeerCapable :: (String, SendPort ProcessId) -> Process ()
onPeerCapable (service, replyTo) = do
    maySay $ "Capability request: " ++ service
    res <- whereis service
    case res of
        Nothing -> maySay "I can't."
        Just pid -> maySay "I can!" >> sendChan replyTo pid

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




--

data NewPeer = NewPeer NodeId
  deriving (Generic)

instance Binary NewPeer



peerListenerService :: String
peerListenerService = "P2P::Listener"

peerNotifier :: Process ()
peerNotifier = do
  getSelfPid >>= register peerListenerService
  f []
  where
    f pids = do
      let fanout m = forM_ pids $ \p -> send p (m :: NewPeer)
      receiveWait [ match fanout, match $ f . (:pids) ]
