module PeerProtocol (
                        download,
                    ) where
import File
import Types
import TypesHelp
import Communicate

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
import System.Log.Logger
import Control.Exception

-- Makes a TCP socket on the port which is used by the UDP socket and then connect to the Peer
makeTCPSock :: Socket -> SockAddr -> IO (Maybe Socket)
makeTCPSock sockUDP peerAddr = do bindAddr <- getSocketName sockUDP
                                  sockTCP <- socket AF_INET Stream defaultProtocol
                                  setSocketOption sockTCP ReuseAddr 1
                                  setSocketOption sockTCP ReusePort 1
                                  bind sockTCP bindAddr
                                  infoM "HasTorrent" $ "About to connect to "++ show peerAddr
                                  mayConnect <- timeout 1000000 $ connect sockTCP peerAddr
                                  case mayConnect of Nothing -> do close sockTCP
                                                                   infoM "HasTorrent" $ "Not Connected to Peer "++ show peerAddr
                                                                   return Nothing
                                                     Just _  -> do infoM "HasTorrent" $ "Connected to Peer "++ show peerAddr
                                                                   return $ Just sockTCP

-- Sends a successful handshake to a peer
sendHandshake :: Peer -> Stateless -> IO Peer
sendHandshake peer@(NoHandshakeSent peerAddr) constants = do poss <- try (makeTCPSock (getUDPSocket constants) peerAddr) :: IO (Either SomeException (Maybe Socket))
                                                             case poss of
                                                               Left _ -> return peer
                                                               Right maybeSock ->
                                                                  if isNothing maybeSock
                                                                  then return peer
                                                                  else do let sockPeer = fromJust maybeSock
                                                                          infoM "HasTorrent" $ "About to send Handshake to "++ show peerAddr
                                                                          send sockPeer (toLazyByteString $ execWriter $ do
                                                                                                 tell $ int8 pStrLen
                                                                                                 tell $ string8 pStr
                                                                                                 tell $ lazyByteString reservedBytes
                                                                                                 tell $ lazyByteString $ getHash $ getInfoHash constants
                                                                                                 tell $ lazyByteString $ getHash $ getPeerId constants)
                                                                          infoM "HasTorrent" $ "Handshake sent to "++ show peerAddr
                                                                          return $ NoHandshakeRecvd peerAddr sockPeer BL.empty
sendHandshake peer _ = return peer

-- Receives a handshake from the peer, parses it (if possible) and returns the new peer
recvHandshake :: Peer -> Stateless -> IO Peer
recvHandshake peer@(NoHandshakeRecvd {}) constants = do peer <- recvDataPeer peer
                                                        case peer of
                                                            NoHandshakeSent{} -> return peer
                                                            _ -> do
                                                               let (handshakeSucc,rest) = runState (parseHandshake constants) $ getUnparsed peer
                                                               infoM "HasTorrent" "Tried receiving handshake"
                                                               if handshakeSucc
                                                               then do time <- getCurrentTime
                                                                       infoM "HasTorrent" $ "Handshake successfully received from "++ show (getPeerAddress peer)
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

verifyHashAndWrite :: Int -> Stateless -> StateT Torrent IO ()
verifyHashAndWrite index constants = do torrent <- get
                                        if computedHash torrent index == expectedHash constants index
                                        then do lift $ forkIO $ writePiece index constants torrent
                                                lift $ infoM "HasTorrent" $ "Piece number "++ show index ++" has been written to disk"
                                                let pieces = (V.//) (getPieces torrent) [(index,setVerifiedStatus $ getPieces torrent V.! index)]
                                                put torrent{getPieces = pieces}
                                        else do let pieces = (V.//) (getPieces torrent) [(index,erasePieceData $ getPieces torrent V.! index)]
                                                lift $ infoM "HasTorrent" $ "Piece number "++ show index ++" hash failed"
                                                put torrent{getPieceDownOrd = getPieceDownOrd torrent ++ [index], getPieces = pieces}

download :: Stateless -> StateT Torrent IO ()
download = checkAndAddPieces

checkAndAddPieces :: Stateless -> StateT Torrent IO ()
checkAndAddPieces constants = do torrent <- get
                                 lift $ debugM "HasTorrent" $ show $ getActiveBlocks torrent
                                 let numActiveBlocks = S.size $ getActiveBlocks torrent
                                 let pieceOrd = getPieceDownOrd torrent
                                 if numActiveBlocks >= minActiveBlocks || null pieceOrd
                                 then do lift $ infoM "HasTorrent" $ "Number of pieces left: " ++ show (length pieceOrd)
                                         lift $ infoM "HasTorrent" $ "Number of active blocks left: " ++ show numActiveBlocks
                                         unless (numActiveBlocks == 0) $ activePeer constants
                                 else do let x:xs = pieceOrd
                                         let newBlocks = pieceToReqs x $ getPieces torrent
                                         put torrent{getPieceDownOrd = xs, getActiveBlocks = S.union (getActiveBlocks torrent) newBlocks}
                                         checkAndAddPieces constants

makeRequests :: S.Set RequestId -> StateT Torrent IO ()
makeRequests reqFrom = do torrent <- get
                          let peer = Z.cursor $ getActivePeers torrent
                          unless (length (getRequestList peer) > minPeerRequests || S.size reqFrom == 0) $ do
                              id <- lift $ randomRIO (0, S.size reqFrom - 1)
                              let req = S.elemAt id reqFrom
                              lift $ send (getSocket peer) $ msgToByteStr $ toReqMsg req
                              time <- lift getCurrentTime
                              peer <- return $ peer{getRequestList = req:getRequestList peer,
                                                    getRequestTime = time}
                              put torrent{getActivePeers = Z.replace peer $ getActivePeers torrent}
                              makeRequests $ S.deleteAt id reqFrom

activeSend :: Stateless -> StateT Torrent IO ()
activeSend constants = do torrent <- get
                          let peer = Z.cursor $ getActivePeers torrent
                          let (completedList,pendingList) = L.partition (`S.notMember` getActiveBlocks torrent) $ getRequestList peer
                          peer <- return $ peer{getRequestList = pendingList}
                          put torrent{getActivePeers = Z.replace peer $ getActivePeers torrent}
                          torrent <- get
                          lift $ forM_ completedList $ send (getSocket peer) . msgToByteStr . toCanMsg
                          time <- lift getCurrentTime
                          if diffUTCTime time (getEffResponseTime peer) >= effResponseTime
                          then put torrent{getActivePeers = Z.delete $ getActivePeers torrent,
                                           getInactivePeers = Z.push peer $ getInactivePeers torrent}
                          else do let reqFrom = foldr S.delete (getActiveBlocks torrent) pendingList
                                  makeRequests reqFrom
                                  torrent <- get
                                  let peer = Z.cursor $ getActivePeers torrent
                                  time <- lift getCurrentTime
                                  when (diffUTCTime time (getRequestTime peer) >= requestTime) $ do
                                      lift $ send (getSocket peer) $ msgToByteStr KeepAliveMsg
                                      peer <- return $ peer{getRequestTime = time}
                                      put torrent{getActivePeers = Z.replace peer $ getActivePeers torrent}

inactiveSend :: Stateless -> StateT Torrent IO ()
inactiveSend constants = do torrent <- get
                            let peer = Z.cursor $ getInactivePeers torrent
                            time <- lift getCurrentTime
                            if diffUTCTime time (getResponseTime peer) >= responseTime
                            then do lift $ close $ getSocket peer
                                    peer <- return $ NoHandshakeSent $ getPeerAddress peer
                                    put torrent{getInactivePeers = Z.replace peer $ getInactivePeers torrent}
                            else if not $ getAmInterested $ getPeerState peer
                                 then do lift $ send (getSocket peer) $ msgToByteStr InterestedMsg
                                         let peerstate = (getPeerState peer){getAmInterested = True}
                                         peer <- return $ peer{getPeerState = peerstate, getRequestTime = time}
                                         put torrent{getInactivePeers = Z.replace peer $ getInactivePeers torrent}
                                 else when (diffUTCTime time (getRequestTime peer) >= requestTime) $ do
                                             lift $ send (getSocket peer) $ msgToByteStr KeepAliveMsg
                                             peer <- return $ peer{getRequestTime = time}
                                             put torrent{getInactivePeers = Z.replace peer $ getInactivePeers torrent}

activePeer :: Stateless -> StateT Torrent IO ()
activePeer constants = do torrent <- get
                          lift $ debugM "HasTorrent" "Active Peer"
                          if Z.endp $ getActivePeers torrent
                              then do put torrent{getActivePeers = Z.start $ getActivePeers torrent}
                                      inactivePeer constants
                              else do let peer = Z.cursor $ getActivePeers torrent
                                      lift $ debugM "HasTorrent" $ show peer
                                      peer <- lift $ recvDataPeer peer
                                      case peer of
                                        Handshake{} -> do peer <- return $ execState peerMessages peer
                                                          put torrent{getActivePeers = Z.replace peer $ getActivePeers torrent}
                                                          success <- processedActiveMessages constants
                                                          when success $ do
                                                              activeSend constants
                                                              torrent <- get
                                                              put torrent{getActivePeers = Z.right $ getActivePeers torrent}
                                                          activePeer constants
                                        _ -> put torrent{getInactivePeers = Z.push peer $ getInactivePeers torrent,
                                                         getActivePeers = Z.delete $ getActivePeers torrent}

inactivePeer :: Stateless -> StateT Torrent IO ()
inactivePeer constants = do torrent <- get
                            lift $ debugM "HasTorrent" "Inactive Peer"
                            if Z.endp $ getInactivePeers torrent
                                then do put torrent{getInactivePeers = Z.start $ getInactivePeers torrent}
                                        checkAndAddPieces constants
                                else do let peer = Z.cursor $ getInactivePeers torrent
                                        lift $ debugM "HasTorrent" $ show peer
                                        case peer of
                                             NoHandshakeSent {} -> do peer <- lift $ sendHandshake peer constants
                                                                      put torrent{getInactivePeers = Z.right $ Z.replace peer $ getInactivePeers torrent}
                                             NoHandshakeRecvd {} -> do peer <- lift $ recvHandshake peer constants
                                                                       put torrent{getInactivePeers = Z.right $ Z.replace peer $ getInactivePeers torrent}
                                             Handshake{} -> do peer <- lift $ recvDataPeer peer
                                                               case peer of
                                                                 Handshake{} -> do
                                                                     peer <- return $ execState peerMessages peer
                                                                     put torrent{getInactivePeers = Z.replace peer $ getInactivePeers torrent}
                                                                     success <- processedInactiveMessages constants
                                                                     when success $ do
                                                                         lift $ debugM "HasTorrent" "About to send messages"
                                                                         inactiveSend constants
                                                                         torrent <- get
                                                                         put torrent{getInactivePeers = Z.right $ getInactivePeers torrent}
                                                                 _ -> put torrent{getInactivePeers = Z.right $ Z.replace peer $ getInactivePeers torrent}
                                        inactivePeer constants


processedInactiveMessages :: Stateless -> StateT Torrent IO Bool
processedInactiveMessages constants = do torrent <- get
                                         let peer = Z.cursor $ getInactivePeers torrent
                                         let messages = getPendingMessages peer
                                         if null messages then return True
                                         else do
                                            let (msg:rest) = messages
                                            lift $debugM "HasTorrent" $ "Message Logged: " ++ show msg
                                            time <- lift getCurrentTime
                                            peer <- return $ peer{getResponseTime = time}
                                            case msg of ChokeMsg -> do
                                                            peer <- return $ peer{getEffResponseTime=time,
                                                                                  getPeerState = (getPeerState peer){getPeerChoking=True},
                                                                                  getRequestList = [],
                                                                                  getPendingMessages = rest}
                                                            put torrent{getInactivePeers = Z.replace peer $ getInactivePeers torrent}
                                                            processedInactiveMessages constants

                                                        UnchokeMsg -> do
                                                            peer <- return $ peer{getEffResponseTime=time,
                                                                                  getPeerState = (getPeerState peer){getPeerChoking=False},
                                                                                  getPendingMessages = rest}
                                                            if getAmInterested (getPeerState peer) && zipLength (getActivePeers torrent) <= maxActivePeers
                                                            then do put torrent{getInactivePeers = Z.delete $ getInactivePeers torrent,
                                                                                getActivePeers = Z.push peer $ getActivePeers torrent}
                                                                    return False
                                                            else do put torrent{getInactivePeers = Z.replace peer $ getInactivePeers torrent}
                                                                    processedInactiveMessages constants

                                                        PieceMsg{} -> do
                                                            peer <- return $ peer{getEffResponseTime=time,
                                                                                  getPendingMessages = messages}
                                                            if zipLength (getActivePeers torrent) <= maxActivePeers
                                                            then do put torrent{getInactivePeers = Z.delete $ getInactivePeers torrent,
                                                                                getActivePeers = Z.push peer $ getActivePeers torrent}
                                                                    return False
                                                            else do put torrent{getInactivePeers = Z.right $ Z.replace peer $ getInactivePeers torrent}
                                                                    return False

                                                        _ -> do
                                                            lift $ debugM "HasTorrent" "Fail"
                                                            peer <- return $ peer{getPendingMessages = rest}
                                                            put torrent{getInactivePeers = Z.replace peer $ getInactivePeers torrent}
                                                            processedInactiveMessages constants

recvdBlockData :: Message -> Stateless -> StateT Torrent IO ()
recvdBlockData msg constants = do torrent <- get
                                  lift $ debugM "HasTorrent" "Block data recd."
                                  let peer = Z.cursor $ getActivePeers torrent
                                  when (getPieceIndex msg < V.length (getPieces torrent)) $ do
                                      let piece = getPieces torrent V.! getPieceIndex msg
                                      let maybeBlock = M.lookup (getBlockBegin msg) $ getBlocks piece
                                      unless (isNothing maybeBlock) $ do
                                          let block = fromJust maybeBlock
                                          let recvId = RequestId (getPieceIndex msg,getBlockBegin msg,getBlockLength msg)
                                          when (elem recvId $ getRequestList peer) $ do
                                              peer <- return $ peer{getRequestList = L.delete recvId $ getRequestList peer}
                                              when (S.member recvId $ getActiveBlocks torrent) $ do
                                                  block <-return$ block {getDownloadStatus = True, getData = getBlock msg}
                                                  piece <-return$ piece {getBlocks = M.adjust (const block) (getBlockBegin msg) $ getBlocks piece}
                                                  put torrent{getActiveBlocks = S.delete recvId $ getActiveBlocks torrent,
                                                              getActivePeers = Z.replace peer $ getActivePeers torrent,
                                                              getPieces = getPieces torrent V.// [(getPieceIndex msg,piece)]}
                                                  when (M.foldr (\a b -> getDownloadStatus a && b) True $ getBlocks piece) $ verifyHashAndWrite (getPieceIndex msg) constants

processedActiveMessages :: Stateless -> StateT Torrent IO Bool
processedActiveMessages constants = do torrent <- get
                                       let peer = Z.cursor $ getActivePeers torrent
                                       let messages = getPendingMessages peer
                                       if null messages then return True
                                       else do
                                         let (msg:rest) = messages
                                         time <- lift getCurrentTime
                                         peer <- return $ peer {getPendingMessages = rest, getResponseTime = time}
                                         case msg of ChokeMsg -> do peer <- return$ peer {getEffResponseTime = time,
                                                                                          getPeerState = (getPeerState peer){getPeerChoking=True},
                                                                                          getRequestList = []}
                                                                    put torrent {getActivePeers = Z.delete $ getActivePeers torrent,
                                                                                 getInactivePeers = Z.push peer $ getInactivePeers torrent}
                                                                    return False
                                                     PieceMsg{} -> do peer <- return $ peer {getEffResponseTime = time}
                                                                      put torrent {getActivePeers = Z.replace peer $ getActivePeers torrent}
                                                                      recvdBlockData msg constants
                                                                      processedActiveMessages constants
                                                     _ -> processedActiveMessages constants
