module PeerProtocol where
import File
import Types
import TypesHelp
import Communicate
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Writer
import Crypto.Hash.SHA1
import Data.Byteable
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
import Control.Applicative
import Data.Time

makeTCPSock :: Socket -> SockAddr -> IO Socket
makeTCPSock sockUDP peerAddr = do bindAddr <- getSocketName sockUDP
                                  sockTCP <- socket AF_INET Stream defaultProtocol
                                  setSocketOption sockTCP ReuseAddr 1
                                  bind sockTCP bindAddr
                                  connect sockTCP peerAddr
                                  return sockTCP

sendHandshake :: Peer -> Stateless -> IO Peer
sendHandshake (NoHandshakeSent peerAddr) constants = do sockPeer <- makeTCPSock (getUDPSocket constants) peerAddr
                                                        send sockPeer (toLazyByteString $ execWriter $ do
                                                                                    tell $ int8 pStrLen
                                                                                    tell $ string8 pStr
                                                                                    tell $ lazyByteString reservedBytes
                                                                                    tell $ lazyByteString $ getHash $ getInfoHash constants
                                                                                    tell $ lazyByteString $ getHash $ getPeerId constants)
                                                        return $ NoHandshakeRecvd peerAddr sockPeer BL.empty
sendHandshake peer _ = return peer

recvHandshake :: Peer -> Stateless -> IO Peer
recvHandshake peer@(NoHandshakeRecvd {}) constants = do peer <- recvDataPeer peer
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
recvDataPeer peer= do mayRecvd <- timeout 1000 $ recv (getSocket peer) (1024 * 128)
                      case mayRecvd of Nothing -> return peer
                                       (Just recvd) -> appendDataPeer peer recvd

expectedHash :: Stateless -> Int -> BL.ByteString
expectedHash constants index = getHash $ getPieceHash $ getPieceInfo constants V.! index

computedHash :: Torrent -> Int -> BL.ByteString
computedHash torrent index = BL.fromStrict $ toBytes $ hashlazy $ getPieceData $ getPieces torrent V.! index

eraseBlockData :: Block -> Block
eraseBlockData block = block{ getDownloadStatus = False, getData = BL.empty }

erasePieceData :: Piece -> Piece
erasePieceData piece = piece{ getBlocks = M.map eraseBlockData $ getBlocks piece }

setVerifiedStatus :: Piece -> Piece
setVerifiedStatus piece = piece{ getVerifiedStatus = True }

verifyHashAndWrite :: Int -> Stateless -> StateT Torrent IO ()
verifyHashAndWrite index constants = StateT $ \torrent -> if computedHash torrent index == expectedHash constants index
                                                          then do forkIO $ writePiece index constants torrent
                                                                  let pieces = (V.//) (getPieces torrent) [(index,setVerifiedStatus $ getPieces torrent V.! index)]
                                                                  return ((),torrent{ getPieces = pieces })
                                                          else do let pdo = getPieceDownOrd torrent
                                                                  let pieces = (V.//) (getPieces torrent) [(index,erasePieceData $ getPieces torrent V.! index)]
                                                                  return ((),torrent{ getPieceDownOrd = pdo ++ [index], getPieces = pieces })

checkAndAddPieces :: StateT Torrent IO ()
checkAndAddPieces = do torrent <- get
                       let numActiveBlocks = S.size $ getActiveBlocks torrent
                       let pieceOrd = getPieceDownOrd torrent
                       unless (numActiveBlocks >= minActiveBlocks || null pieceOrd) $ do
                            let x:xs = pieceOrd
                            let newBlocks = pieceToReqs x $ getPieces torrent
                            put torrent{getPieceDownOrd = xs, getActiveBlocks = S.union (getActiveBlocks torrent) newBlocks}
                            checkAndAddPieces

activePeer :: Stateless -> StateT Torrent IO ()
activePeer constants = do torrent <- get
                          if Z.endp $ getActivePeers torrent
                              then do put torrent{getActivePeers = Z.start $ getActivePeers torrent}
                                      -- TODO: inactivePeer
                              else do let peer = Z.cursor $ getActivePeers torrent
                                      peer <- lift $ recvDataPeer peer
                                      put torrent{getActivePeers = Z.replace (execState peerMessages peer) $ getActivePeers torrent}
                                      success <- processedActiveMessages constants
                                      when success $ do return ()
                                          --TODO : Check if Inactive(by time of response and send the required messages)

recvdBlockData :: Message -> Stateless -> StateT Torrent IO ()
recvdBlockData msg constants = do torrent <- get
                                  let peer = Z.cursor $ getActivePeers torrent
                                  when (getPieceIndex msg < V.length (getPieces torrent)) $ do
                                      let piece = getPieces torrent V.! getPieceIndex msg
                                      let maybeBlock = M.lookup (getBlockBegin msg) $ getBlocks piece
                                      unless (isNothing maybeBlock) $ do
                                          let block = fromJust maybeBlock
                                          let recvId = RequestId (getPieceIndex msg,getBlockBegin msg,getBlockLength msg)
                                          when (elem recvId $ getRequestList peer) $ do
                                              let peer = peer{getRequestList = L.delete recvId $ getRequestList peer}
                                              when (S.member recvId $ getActiveBlocks torrent) $ do
                                                  let block = block {getDownloadStatus = True, getData = getBlock msg}
                                                  let piece = piece {getBlocks = M.adjust (const block) (getBlockBegin msg) $ getBlocks piece}
                                                  put torrent{getActiveBlocks = S.delete recvId $ getActiveBlocks torrent,
                                                              getActivePeers = Z.replace peer $ getActivePeers torrent,
                                                              getPieces = getPieces torrent V.// [(getPieceIndex msg,piece)]}
                                                  when (M.foldr (\a b -> getDownloadStatus a && b) True $ getBlocks piece) $ verifyHashAndWrite (getPieceIndex msg) constants

processedActiveMessages :: Stateless -> StateT Torrent IO Bool
processedActiveMessages constants = do torrent <- get
                                       let peer = Z.cursor $ getActivePeers torrent
                                       let messages = getPendingMessages peer
                                       if null messages then return True
                                       else do let (msg:rest) = messages
                                               time <- lift getCurrentTime
                                               let peer = peer {getPendingMessages = rest, getResponseTime = time}
                                               case msg of ChokeMsg -> do let peer = peer {getEffResponseTime = time,
                                                                                           getPeerState = (getPeerState peer){getPeerChoking=True},
                                                                                           getRequestList = []}
                                                                          put torrent {getActivePeers = Z.delete $ getActivePeers torrent,
                                                                                       getInactivePeers = Z.push peer $ getInactivePeers torrent}
                                                                          return False
                                                           PieceMsg{} -> do let peer = peer {getEffResponseTime = time}
                                                                            put torrent {getActivePeers = Z.replace peer $ getActivePeers torrent}
                                                                            recvdBlockData msg constants
                                                                            processedActiveMessages constants
                                                           _ -> processedActiveMessages constants
