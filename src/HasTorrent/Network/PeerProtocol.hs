module HasTorrent.Network.PeerProtocol (
                        sendHandshake,
                        recvHandshake,
                        verifyHashAndWrite,
                        recvDataPeer,
                    ) where
import HasTorrent.File
import HasTorrent.Types
import HasTorrent.Types.TypesHelp
import HasTorrent.Network.Communicate

import Control.Concurrent
import Control.Monad.State
import Control.Monad.Writer
import Crypto.Hash.SHA1
import Data.ByteString.Builder
import Data.Int
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.List.Zipper as Z
import qualified Data.List as L
import Data.Word
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import qualified Data.Map.Lazy as M
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import System.Timeout
import System.Random
import Control.Applicative
import Data.Time
import Control.Exception

-- Makes a TCP socket on the port which is used by the UDP socket and then connect to the Peer
makeTCPSock :: Socket -> SockAddr -> IO (Maybe Socket)
makeTCPSock sockUDP peerAddr = do bindAddr <- getSocketName sockUDP
                                  sockTCP <- socket AF_INET Stream defaultProtocol
                                  setSocketOption sockTCP ReuseAddr 1
                                  setSocketOption sockTCP ReusePort 1
                                  bind sockTCP bindAddr

                                  mayConnect <- timeout 1000000 $ connect sockTCP peerAddr
                                  case mayConnect of Nothing -> do close sockTCP
                                                                   return Nothing
                                                     Just _  -> return $ Just sockTCP

-- Sends a successful handshake to a peer
sendHandshake :: Peer -> Stateless -> IO Peer
sendHandshake peer@(NoHandshakeSent peerAddr) constants = do poss <- try (makeTCPSock (getUDPSocket constants) peerAddr) :: IO (Either SomeException (Maybe Socket))
                                                             case poss of
                                                               Left e -> return peer
                                                               Right maybeSock ->
                                                                  if isNothing maybeSock
                                                                  then return peer
                                                                  else do let sockPeer = fromJust maybeSock
                                                                          send sockPeer (toLazyByteString $ execWriter $ do
                                                                                                 tell $ int8 pStrLen
                                                                                                 tell $ string8 pStr
                                                                                                 tell $ lazyByteString reservedBytes
                                                                                                 tell $ lazyByteString $ getHash $ getInfoHash constants
                                                                                                 tell $ lazyByteString $ getHash $ getPeerId constants)
                                                                          return $ NoHandshakeRecvd peerAddr sockPeer BL.empty
sendHandshake peer _ = return peer

-- Receives a handshake from the peer, parses it (if possible) and returns the new peer
recvHandshake :: Peer -> Stateless -> IO Peer
recvHandshake peer@(NoHandshakeRecvd {}) constants = do peer <- recvDataPeer peer
                                                        case peer of
                                                            NoHandshakeSent{} -> return peer
                                                            _ -> do
                                                               let (handshakeSucc,rest) = runState (parseHandshake constants) $ getUnparsed peer
                                                               if handshakeSucc
                                                               then do time <- getCurrentTime
                                                                       return $ Handshake (getPeerAddress peer) initPeerState (getSocket peer) rest [] time time time []
                                                               else return peer
recvHandshake peer _ = return peer

appendDataPeer :: Peer -> BL.ByteString -> IO Peer
appendDataPeer (NoHandshakeSent sockaddr) _ = return $ NoHandshakeSent sockaddr
appendDataPeer peer recvd = if recvd == BL.empty then do close $ getSocket peer
                                                         return $ NoHandshakeSent $ getPeerAddress peer
                                                 else return $ peer{getUnparsed = BL.append (getUnparsed peer) recvd}

recvDataPeer :: Peer -> IO Peer
recvDataPeer (NoHandshakeSent sockaddr) = return $ NoHandshakeSent sockaddr
recvDataPeer peer= do mayRecvd <- timeout 100000 $ recv (getSocket peer) (1024 * 128)
                      case mayRecvd of Nothing -> return peer
                                       (Just recvd) -> appendDataPeer peer recvd

-- Verifies if the hash of the piece received matches that given in the torrent file and accordingly writes to disk or resets the piece
verifyHashAndWrite :: Int -> Stateless -> StateT Torrent IO ()
verifyHashAndWrite index constants = do torrent <- get
                                        if computedHash torrent index == expectedHash constants index
                                        then do lift $ forkIO $ writePiece index constants torrent
                                                let pieces = (V.//) (getPieces torrent) [(index,setVerifiedStatus $ getPieces torrent V.! index)]
                                                put torrent{getPieces = pieces}
                                        else do let pieces = (V.//) (getPieces torrent) [(index,erasePieceData $ getPieces torrent V.! index)]
                                                put torrent{getPieceDownOrd = getPieceDownOrd torrent ++ [index], getPieces = pieces}
