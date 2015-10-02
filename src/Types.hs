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

type AnnounceURL = [Char]
type AnnounceList = [AnnounceURL]

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
data Tracker = Tracker {
                         getURL :: AnnounceURL,
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
data Peer = NoHandshake
          | Handshake PeerState Socket
type PeerList = V.Vector Peer

data Torrent = Torrent {
                         getPieces :: PieceList,
                         getTracker :: Tracker,
                         getPeers :: PeerList
                       }
