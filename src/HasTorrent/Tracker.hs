module HasTorrent.Tracker where
import HasTorrent.Types
import HasTorrent.Tracker.UDPTracker
import HasTorrent.Tracker.HTTPTracker

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import qualified Data.Vector as V
import qualified Data.List.Zipper as Z
import Data.Binary(decode)
import qualified Data.ByteString.Lazy.Char8 as LC
import System.Log.Logger
import Data.Word
import Network.Socket

-- Parse the peers from the tracker (UDP) announce response
extractPeers :: LC.ByteString -> PeerList
extractPeers peerRes = if LC.length peerRes < 6 then Z.empty
                                                else let (first,rest) = LC.splitAt 6 peerRes
                                                         (ip,port) = LC.splitAt 4 first
                                                     in NoHandshakeSent (SockAddrInet (fromIntegral (decode port :: Word16)) (decode (LC.reverse ip) :: Word32)) `Z.insert` extractPeers rest

-- |Decide which tracker to use and call the independent module code
getPeers :: Tracker -> Stateless -> Torrent -> IO PeerList
getPeers udpTracker@UDPTracker{} constants torrent = do bytes <- getPeersUDP udpTracker constants torrent
                                                        return $ extractPeers bytes
getPeers httpTracker@HTTPTracker{} constants torrent = do bytes <- getResponse httpTracker constants torrent
                                                          return $ extractPeers bytes

-- |Finds and adds only the unique peers, i.e. ones which are not in the active and inactive lists
addUniquePeers :: PeerList -> StateT Torrent IO Int
addUniquePeers peers = do torrent <- get
                          let active = Z.toList $ getActivePeers torrent
                          let inactive = Z.toList $ getInactivePeers torrent
                          let new = Z.toList peers
                          let unique = (new \\ active) \\ inactive
                          put torrent{getInactivePeers = Z.fromList $ inactive ++ unique}
                          return $ length unique

-- |If the old tracker doesn't give new peers then decide a new tracker based on unique peers we get
findNewTracker :: Stateless -> TrackerList -> StateT Torrent IO ()
findNewTracker constants [] = return ()
findNewTracker constants (tracker:xs) = do torrent <- get
                                           peers <- lift $ getPeers tracker constants torrent
                                           new <- addUniquePeers peers
                                           if new == 0 then findNewTracker constants xs
                                           else do torrent <- get
                                                   put torrent{getActiveTracker = Just tracker}

-- |Add new peers (if possible) and change the state of the PeerLists and the Active Tracker appropriately
findAndAddPeers :: Stateless -> StateT Torrent IO ()
findAndAddPeers constants = do torrent <- get
                               case getActiveTracker torrent of
                                  Nothing -> findNewTracker constants (getTrackers constants)
                                  Just tracker -> do peers <- lift $ getPeers tracker constants torrent
                                                     new <- addUniquePeers peers
                                                     when (new == 0) $ do
                                                        put torrent{getActiveTracker = Nothing}
                                                        findAndAddPeers constants
