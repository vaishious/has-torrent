module Types where
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Network.Socket
import System.IO

data File = File {
                   getFilePath :: FilePath,
                   getSize :: Integer
                 }
type FileList = V.Vector File

type Hash = BL.ByteString
type PieceHash = V.Vector Hash
data PieceInfo = PieceInfo {
                             getPieceLength :: Int,
                             getPieceHash :: PieceHash
                           }

type PeerId = BL.ByteString

type Port = Int
type AnnounceURL = String
type AnnounceList = [(AnnounceURL,Port)]

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

type ConnectionId = BL.ByteString

data TrackerProtocol = HTTP | UDP
data Tracker = Tracker {
                         getProtocol :: TrackerProtocol,    --
                         getURL :: AnnounceURL,             -- Are these three lines required
                         getPort :: Port,                   --
                         getSocket :: Socket,
                         getConnId :: ConnectionId
                       }
             | NoTracker

data PeerState = PeerState {
                             getAmChoking :: Bool,
                             getAmInterested :: Bool,
                             getPeerChoking :: Bool,
                             getPeerInterested :: Bool
                           }
data Peer = NoHandshake HostAddress Port       -- HostAddress is defined in Network.Socket as a Word32
          | Handshake PeerState Socket
type PeerList = V.Vector Peer

data Torrent = Torrent {
                         getPieces :: PieceList,
                         getTracker :: Tracker,
                         getPeers :: PeerList
                       }
