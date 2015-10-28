module UDPTracker where
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.BSD
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder
import Data.Binary
import Data.Int
import Data.Word
import Data.Monoid
import Data.Maybe
import qualified Data.List.Zipper as Z
import qualified Data.Vector as V
import System.Timeout
import System.Random
import Control.Monad
import Control.Monad.Writer
import Types
import TypesHelp

--TODO: Handle Exceptions?
makeSockAddr :: Tracker -> IO SockAddr
makeSockAddr (UDPTracker hostName port) = do trackerIP <- getHostByName hostName
                                             return $ SockAddrInet (fromIntegral port) (hostAddress trackerIP)

connectConnId :: Int64
connectConnId = 4497486125440

connectAction :: Int32
connectAction = 0

announceAction :: Int32
announceAction = 1

toStrictByteString :: Builder -> B.ByteString
toStrictByteString = BL.toStrict . toLazyByteString

sendConnectReq :: Socket -> SockAddr -> Int -> IO (Maybe BL.ByteString)
sendConnectReq sockUDP trackerAddr timeWaitSec = do transIdReq <- randomIO
                                                    sendTo sockUDP (toStrictByteString $ execWriter $ do tell $ int64BE connectConnId
                                                                                                         tell $ int32BE connectAction
                                                                                                         tell $ int32BE transIdReq) trackerAddr
                                                    maybeResponse <- timeout (timeWaitSec*1000000) $ recv sockUDP 16
                                                    return $ liftM BL.fromStrict maybeResponse >>= validConnectRes transIdReq

validConnectRes :: Int32 -> BL.ByteString -> Maybe BL.ByteString
validConnectRes transIdReq response = do guard $ BL.length response >= 16
                                         let (actionRes,rest) = BL.splitAt 4 response
                                             (transIdRes,connIdRes) = BL.splitAt 4 rest
                                         guard $ (decode actionRes :: Int32) == connectAction
                                         guard $ (decode transIdRes :: Int32) == transIdReq
                                         return connIdRes

--sendAnnounceReq :: Tracker -> PieceList -> Hash -> PeerId -> Int -> IO PeerList
--sendAnnounceReq tracker pieces infoHash peerId timeWaitSec = do transIdReq <- randomIO
sendAnnounceReq :: BL.ByteString -> SockAddr -> Stateless -> Int -> Torrent -> IO PeerList
sendAnnounceReq connId trackerAddr constants timeWaitSec stateful = do transIdReq <- randomIO
                                                                       key <- randomIO
                                                                       port <- socketPort $ getUDPSocket constants
                                                                       sendTo (getUDPSocket constants) (toStrictByteString $ execWriter $ do
                                                                            tell $ lazyByteString connId
                                                                            tell $ int32BE announceAction
                                                                            tell $ int32BE transIdReq
                                                                            tell $ lazyByteString $ getHash $ getInfoHash constants
                                                                            tell $ lazyByteString $ getHash $ getPeerId constants
                                                                            tell $ int64BE $ getDownload $ getPieces stateful
                                                                            tell $ int64BE $ getLeft $ getPieces stateful
                                                                            -- Uploaded = 0?
                                                                            tell $ int64BE 0
                                                                            tell $ int32BE $ decodeEvent $ getEvent stateful
                                                                            tell $ int32BE 0
                                                                            tell $ int32BE key
                                                                            tell $ int64BE (-1)
                                                                            tell $ int16BE $ fromIntegral port) trackerAddr
                                                                       maybeResponse <- timeout (timeWaitSec*100000) $ recv (getUDPSocket constants) 320
                                                                       case liftM BL.fromStrict maybeResponse >>= validAnnounceRes transIdReq of
                                                                            Nothing -> return Z.empty
                                                                            Just peerRes -> return $ extractPeers peerRes

validAnnounceRes :: Int32 -> BL.ByteString -> Maybe BL.ByteString
validAnnounceRes transIdReq response = do guard $ BL.length response >=20
                                          let (actionRes,rest) = BL.splitAt 4 response
                                              (transIdRes,details) = BL.splitAt 4 rest
                                          guard $ (decode actionRes :: Int32) == announceAction
                                          guard $ (decode transIdRes :: Int32) == transIdReq
                                          return $ BL.drop 12 details

extractPeers :: BL.ByteString -> PeerList
extractPeers peerRes = if BL.length peerRes < 6 then Z.empty
                                                else let (first,rest) = BL.splitAt 6 peerRes
                                                         (ip,port) = BL.splitAt 4 first
                                                     in NoHandshakeSent (SockAddrInet (fromIntegral (decode port :: Word16)) (decode ip :: Word32)) `Z.insert` extractPeers rest

getPeers :: Tracker -> Stateless -> Torrent -> IO PeerList
getPeers udpTracker constants stateful = do trackerAddr <- makeSockAddr udpTracker
                                            -- Tracker timeout for each connect set to 30 seconds. Change?
                                            maybeConnect <- sendConnectReq (getUDPSocket constants) trackerAddr 30
                                            case maybeConnect of Nothing -> return Z.empty
                                                                 Just connId -> sendAnnounceReq connId trackerAddr constants 30 stateful
