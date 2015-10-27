module PeerProtocol where
import File
import Types
import TypesHelp
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Writer
import Crypto.Hash.SHA1
import Data.Byteable
import Data.ByteString.Builder
import Data.Int
import qualified Data.Vector as V
import Data.Word
import qualified Data.ByteString.Lazy as BL
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy

pStr :: String
pStr = "BitTorrent protocol"

pStrLen :: Int8
pStrLen = 19

reservedBytes :: BL.ByteString
reservedBytes = BL.replicate 8 0

-- remotePort and ipAddr are those of the peer
makeTCPSock :: Socket -> Int -> Int -> IO Socket
makeTCPSock sockUDP remotePort ipAddr = do bindAddr <- getSocketName sockUDP
                                           sockTCP <- socket AF_INET Stream defaultProtocol
                                           bind sockTCP bindAddr
                                           connect sockTCP $ SockAddrInet (fromIntegral remotePort) $ fromIntegral ipAddr
                                           return sockTCP

initHandshake :: Socket -> Stateless -> IO Int
initHandshake sockTCP constants = send sockTCP $ toLazyByteString $ execWriter $ do tell $ int8 pStrLen
                                                                                    tell $ string8 pStr
                                                                                    tell $ lazyByteString reservedBytes
                                                                                    tell $ lazyByteString $ getHash $ getInfoHash constants
                                                                                    tell $ lazyByteString $ getHash $ getPeerId constants

expectedHash :: Stateless -> Int -> BL.ByteString
expectedHash constants index = getHash $ getPieceHash $ getPieceInfo constants V.! index

computedHash :: Torrent -> Int -> BL.ByteString
computedHash torrent index = BL.fromStrict $ toBytes $ hashlazy $ getPieceData $ getPieces torrent V.! index

eraseBlockData :: Block -> Block
eraseBlockData block = block{ getDownloadStatus = False, getData = BL.empty }

erasePieceData :: Piece -> Piece
erasePieceData piece = piece{ getBlocks = V.map eraseBlockData $ getBlocks piece }

setVerifiedStatus :: Piece -> Piece
setVerifiedStatus piece = piece{ getVerifiedStatus = True }

verifyHashAndWrite :: Int -> Stateless -> StateT Torrent IO Bool
verifyHashAndWrite index constants = StateT $ \torrent -> if computedHash torrent index == expectedHash constants index
                                                          then do forkIO $ writePiece index constants torrent
                                                                  let pieces = (V.//) (getPieces torrent) [(index,setVerifiedStatus $ getPieces torrent V.! index)]
                                                                  return (True,torrent{ getPieces = pieces })
                                                          else do let pdo = getPieceDownOrd torrent
                                                                  let pieces = (V.//) (getPieces torrent) [(index,erasePieceData $ getPieces torrent V.! index)]
                                                                  return (False,torrent{ getPieceDownOrd = pdo ++ [index], getPieces = pieces })
