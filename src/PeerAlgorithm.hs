module PeerAlgorithm (
                        download,
                    ) where
import HasTorrent

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
                              lift $ sendMessagePeer peer $ toReqMsg req
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
                          lift $ forM_ completedList $ sendMessagePeer peer . toCanMsg
                          time <- lift getCurrentTime
                          peer <- return $ peer{getRequestTime = time}
                          if diffUTCTime time (getEffResponseTime peer) >= effResponseTime
                          then put torrent{getActivePeers = Z.delete $ getActivePeers torrent,
                                           getInactivePeers = Z.push peer $ getInactivePeers torrent}
                          else do let reqFrom = foldr S.delete (getActiveBlocks torrent) pendingList
                                  makeRequests reqFrom
                                  torrent <- get
                                  let peer = Z.cursor $ getActivePeers torrent
                                  time <- lift getCurrentTime
                                  when (diffUTCTime time (getRequestTime peer) >= requestTime) $ do
                                      lift $ sendMessagePeer peer KeepAliveMsg
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
                                 then do lift $ sendMessagePeer peer InterestedMsg
                                         let peerstate = (getPeerState peer){getAmInterested = True}
                                         peer <- return $ peer{getPeerState = peerstate, getRequestTime = time}
                                         put torrent{getInactivePeers = Z.replace peer $ getInactivePeers torrent}
                                 else when (diffUTCTime time (getRequestTime peer) >= requestTime) $ do
                                             lift $ sendMessagePeer peer KeepAliveMsg
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
                                        _ -> put torrent{getActivePeers = Z.delete $ getActivePeers torrent}
                                      activePeer constants

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
                                                                      case peer of
                                                                        NoHandshakeSent{} -> do
                                                                            put torrent{getInactivePeers = Z.delete $ getInactivePeers torrent}
                                                                        _ -> do
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
