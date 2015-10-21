module Tracker where
import Types
import UDPTracker
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import qualified Data.Vector as V

findPeers :: Tracker -> Stateless -> StateT Torrent IO ()
findPeers udpTracker@(UDPTracker hostName port) constants = do stateful <- get
                                                               peers <- lift $ getPeers udpTracker constants stateful
                                                               put $ addPeers stateful peers

addPeers :: Torrent -> PeerList -> Torrent
addPeers state peers = let active = V.toList $ getActivePeers state
                           inactive = V.toList $ getInactivePeers state
                           new = V.toList peers
                       in state {getInactivePeers = V.fromList $ inactive ++ ((new \\ active) \\ inactive)}
