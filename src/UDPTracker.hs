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
import qualified Data.Vector as V
import System.Timeout
import System.Random
import Control.Monad
import Control.Monad.Writer
import Types

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

sendConnectReq :: Socket -> Int -> IO (Maybe BL.ByteString)
sendConnectReq sockUDP timeWaitSec = do transIdReq <- randomIO
                                        send sockUDP $ toStrictByteString $ execWriter $ do tell $ int64BE connectConnId
                                                                                            tell $ int32BE connectAction
                                                                                            tell $ int32BE transIdReq
                                        maybeResponse <- timeout (timeWaitSec*1000000) $ recv sockUDP 16
                                        return $ liftM BL.fromStrict maybeResponse >>= validConnectRes transIdReq

validConnectRes :: Int32 -> BL.ByteString -> Maybe BL.ByteString
validConnectRes transIdReq response = do guard $ BL.length response >= 16
                                         let (actionRes,rest) = BL.splitAt 4 response
                                             (transIdRes,connIdRes) = BL.splitAt 4 rest
                                         guard $ (decode actionRes :: Int32) == connectAction
                                         guard $ (decode transIdRes :: Int32) == transIdReq
                                         return connIdRes

sendAnnounceReq :: Tracker -> PieceList -> Hash -> PeerId -> Int -> IO PeerList
sendAnnounceReq NoTracker _ _ _ _ = return V.empty
sendAnnounceReq tracker pieces infoHash peerId timeWaitSec = do transIdReq <- randomIO
                                                                key <- randomIO
                                                                port <- socketPort $ getSocket tracker
                                                                send (getSocket tracker) $ toStrictByteString $ execWriter $ do
                                                                    tell $ lazyByteString $ getConnId tracker
                                                                    tell $ int32BE announceAction
                                                                    tell $ int32BE transIdReq
                                                                    tell $ lazyByteString infoHash
                                                                    tell $ lazyByteString peerId
                                                                    tell $ int64BE $ getDownload pieces
                                                                    tell $ int64BE $ getLeft pieces
                                                                    tell $ int64BE 0
                                                                    tell $ int32BE 2
                                                                    tell $ int32BE 0
                                                                    tell $ int32BE key
                                                                    tell $ int64BE (-1)
                                                                    tell $ int16BE $ fromIntegral port
                                                                maybeResponse <- timeout (timeWaitSec*100000) $ recv (getSocket tracker) 320
                                                                case liftM BL.fromStrict maybeResponse >>= validAnnounceRes transIdReq of
                                                                     Nothing -> return V.empty
                                                                     Just peerRes -> return $ extractPeers peerRes

validAnnounceRes :: Int32 -> BL.ByteString -> Maybe BL.ByteString
validAnnounceRes transIdReq response = do guard $ BL.length response >=20
                                          let (actionRes,rest) = BL.splitAt 4 response
                                              (transIdRes,details) = BL.splitAt 4 rest
                                          guard $ (decode actionRes :: Int32) == announceAction
                                          guard $ (decode transIdRes :: Int32) == transIdReq
                                          return $ BL.drop 12 details

extractPeers :: BL.ByteString -> PeerList
extractPeers peerRes = if BL.length peerRes < 6 then V.empty
                                                else let (first,rest) = BL.splitAt 6 peerRes
                                                         (ip,port) = BL.splitAt 4 first
                                                     in NoHandshake (decode ip :: Word32) (decode port :: Word16) `V.cons` extractPeers rest

getPieceDownload :: Piece -> Int
getPieceDownload (Piece _ blocks) = V.foldl (\a b -> if getDownloadStatus b then a + getLength b else a) 0 blocks

getDownload :: PieceList -> Int64
getDownload = V.foldl (\a p -> a + fromIntegral (getPieceDownload p)) 0

getPieceLeft :: Piece -> Int
getPieceLeft (Piece _ blocks) = V.foldl (\a b -> if getDownloadStatus b then a else a + getLength b) 0 blocks

getLeft :: PieceList -> Int64
getLeft = V.foldl (\a p -> a + fromIntegral (getPieceLeft p)) 0
