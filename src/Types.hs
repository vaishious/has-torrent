module Types where
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Network.Socket
import System.IO
import Data.Word
import Data.Time
import Data.Set
import Control.Applicative

data File = File {
                   getFilePath :: FilePath,
                   getSize :: Integer
                 }
type FileList = V.Vector File

newtype Hash = Hash {getHash :: BL.ByteString}

data CoveredFile = CoveredFile {
                                 getCoveredFilePath :: FilePath,
                                 getCoveredFileOffset :: Integer,
                                 getCoveredFileLength :: Int
                               }
type CoveredFileList = V.Vector CoveredFile
data SinglePieceInfo = SinglePieceInfo {
                                         getPieceLength :: Int,
                                         getPieceHash :: Hash,
                                         getCoveredFileList :: CoveredFileList
                                       }
type PieceInfo = V.Vector SinglePieceInfo

-- Temporarily modified UDPHostName to String type
data Tracker = UDPTracker {
                            getUDPHostName :: String,
                            getUDPPort :: PortNumber
                          }
             | HTTPTracker {
                             getURL :: String
                           }
type TrackerList = [Tracker]

data Block = Block {
                     getDownloadStatus :: Bool,
                     getData :: BL.ByteString,
                     getOffset :: Int,
                     getLength :: Int
                   }

data Piece = Piece {
                     getVerifiedStatus :: Bool,
                     getBlocks :: V.Vector Block
                   }
type PieceList = V.Vector Piece

data PeerState = PeerState {
                             getAmChoking :: Bool,
                             getAmInterested :: Bool,
                             getPeerChoking :: Bool,
                             getPeerInterested :: Bool
                           }
data Peer = NoHandshake {
                          getPeerHostAddress :: HostAddress,
                          getPeerPort :: PortNumber
                        }
          | Handshake {
                        getPeerHostAddress :: HostAddress,
                        getPeerPort :: PortNumber,
                        getPeerState :: PeerState,
                        getSocket :: Socket,
                        getRequestTime :: UTCTime,
                        getResponseTime :: UTCTime,
                        getEffResponseTime :: UTCTime,
                        getRequestList :: [RequestId]
                      }
instance Eq Peer where
    p1 == p2 = (getPeerHostAddress p1 == getPeerHostAddress p2) && (getPeerPort p1 == getPeerPort p2)

type PeerList = V.Vector Peer

newtype RequestId = RequestId (Int,Int) deriving (Ord,Eq)
data Event = None | Started | Stopped | Completed deriving (Eq)

data Stateless = Stateless {
                             getInfoHash :: Hash,
                             getPieceInfo :: PieceInfo,
                             getPeerId :: Hash,
                             getTrackers :: TrackerList,
                             getFileList :: FileList,
                             getTCPSocket :: Socket,
                             getUDPSocket :: Socket
                           }

data Torrent = Torrent {
                         getEvent :: Event,
                         getPieces :: PieceList,
                         getPieceDownOrd :: [Int],
                         getActiveBlocks :: Set RequestId,
                         getActiveTracker :: Maybe Tracker,
                         getActivePeers :: PeerList,
                         getInactivePeers :: PeerList
                       }

data Message = KeepAliveMsg
             | ChokeMsg
             | UnchokeMsg
             | InterestedMsg
             | NotInterestedMsg
             | HaveMsg {
                         getPieceIndex :: Int
                       }
             | RequestMsg {
                            getPieceIndex :: Int,
                            getBlockBegin :: Int,
                            getBlockLength :: Int
                          }
             | PieceMsg {
                          getPieceIndex :: Int,
                          getBlockBegin :: Int,
                          getBlockLength :: Int,
                          getBlock :: BL.ByteString
                        }
             | CancelMsg {
                           getPieceIndex :: Int,
                           getBlockBegin :: Int,
                           getBlockLength :: Int
                         }
