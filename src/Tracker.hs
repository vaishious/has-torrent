module Tracker where
import Types
import UDPTracker
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import qualified Data.Vector as V
import qualified Data.List.Zipper as Z

findPeers :: Tracker -> Stateless -> StateT Torrent IO ()
findPeers udpTracker@(UDPTracker hostName port) constants = do stateful <- get
                                                               peers <- lift $ getPeers udpTracker constants stateful
                                                               put $ addPeers stateful peers

addPeers :: Torrent -> PeerList -> Torrent
addPeers state peers = let active = Z.toList $ getActivePeers state
                           inactive = Z.toList $ getInactivePeers state
                           new = Z.toList peers
                       in state {getInactivePeers = Z.fromList $ inactive ++ ((new \\ active) \\ inactive)}
