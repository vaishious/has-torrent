module Types where

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Lazy as M
import Network.Socket
import System.IO
import Data.Word
import Data.Time
import Data.Set
import qualified Data.List.Zipper as Z

data File = File {
                   getFilePath :: FilePath,
                   getSize :: Integer
                 } deriving (Show)
type FileList = V.Vector File

newtype Hash = Hash {getHash :: BL.ByteString} deriving (Show)

data CoveredFile = CoveredFile {
                                 getCoveredFilePath :: FilePath,
                                 getCoveredFileOffset :: Integer,
                                 getCoveredFileLength :: Int
                               } deriving (Show)
type CoveredFileList = V.Vector CoveredFile
data SinglePieceInfo = SinglePieceInfo {
                                         getPieceLength :: Int,
                                         getPieceHash :: Hash,
                                         getCoveredFileList :: CoveredFileList
                                       } deriving (Show)
type PieceInfo = V.Vector SinglePieceInfo

-- Temporarily modified UDPHostName to String type
data Tracker = UDPTracker {
                            getUDPHostName :: String,
                            getUDPPort :: PortNumber
                          }
             | HTTPTracker {
                             getURL :: String
                           } deriving (Show)
type TrackerList = [Tracker]

data Block = Block {
                     getDownloadStatus :: Bool,
                     getData :: BL.ByteString,
                     getOffset :: Int,
                     getLength :: Int
                   } deriving (Show)

data Piece = Piece {
                     getVerifiedStatus :: Bool,
                     getBlocks :: M.Map Int Block
                   } deriving (Show)
type PieceList = V.Vector Piece

data PeerState = PeerState {
                             getAmChoking :: Bool,
                             getAmInterested :: Bool,
                             getPeerChoking :: Bool,
                             getPeerInterested :: Bool
                           } deriving (Show)
data Peer = NoHandshakeSent {
                              getPeerAddress :: SockAddr
                            }
          | NoHandshakeRecvd {
                               getPeerAddress :: SockAddr,
                               getSocket :: Socket,
                               getUnparsed :: BL.ByteString
                             }
          | Handshake {
                        getPeerAddress :: SockAddr,
                        getPeerState :: PeerState,
                        getSocket :: Socket,
                        getUnparsed :: BL.ByteString,
                        getPendingMessages :: [Message],
                        getRequestTime :: UTCTime,
                        getResponseTime :: UTCTime,
                        getEffResponseTime :: UTCTime,
                        getRequestList :: [RequestId]
                      } deriving (Show)

instance Eq Peer where
    p1 == p2 = getPeerAddress p1 == getPeerAddress p2

type PeerList = Z.Zipper Peer

newtype RequestId = RequestId (Int,Int,Int) deriving (Ord,Eq,Show) -- (PieceIndex, BlockOffset, BlockLength)
data Event = None | Started | Stopped | Completed deriving (Eq,Show)

data Stateless = Stateless {
                             getInfoHash :: Hash,
                             getPieceInfo :: PieceInfo,
                             getPeerId :: Hash,
                             getTrackers :: TrackerList,
                             getFileList :: FileList,
                             getTCPSocket :: Socket,
                             getUDPSocket :: Socket
                           } deriving (Show)

data Torrent = Torrent {
                         getEvent :: Event,
                         getPieces :: PieceList,
                         getPieceDownOrd :: [Int],
                         getActiveBlocks :: Set RequestId,
                         getActiveTracker :: Maybe Tracker,
                         getActivePeers :: PeerList,
                         getInactivePeers :: PeerList
                       } deriving (Show)

data Message = KeepAliveMsg
             | ChokeMsg
             | UnchokeMsg
             | InterestedMsg
             | NotInterestedMsg
             | HaveMsg {
                         getPieceIndex :: Int
                       }
             | BitfieldMsg {
                             getBitfield :: BL.ByteString
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
             | PortMsg {
                         getPort :: Int
                       } deriving (Show)
