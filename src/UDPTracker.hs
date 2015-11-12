module UDPTracker (
                      getPeersUDP,
                  ) where
import Types
import TypesHelp

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.BSD
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder
import Data.Binary
import Data.Int
import Data.Word
import Data.Monoid
import Data.Maybe
import Data.Either
import qualified Data.List.Zipper as Z
import qualified Data.Vector as V
import System.Timeout
import System.Random
import Control.Exception
import Control.Monad
import Control.Monad.Writer

connectConnId :: Int64
connectConnId = 4497486125440

connectAction :: Int32
connectAction = 0

announceAction :: Int32
announceAction = 1

-- To resolve the address of the UDP Tracker before connecting
-- Can throw Exception. Need to run using try
makeSockAddr :: Tracker -> IO SockAddr
makeSockAddr (UDPTracker hostName port) = do trackerIP <- getHostByName hostName
                                             return $ SockAddrInet (fromIntegral port) (hostAddress trackerIP)

-- Builder -> Strict ByteString
toStrictByteString :: Builder -> B.ByteString
toStrictByteString = LC.toStrict . toLazyByteString

-- Send a connect request to the UDP Tracker
sendConnectReq :: Socket -> SockAddr -> Int -> IO (Maybe LC.ByteString)
sendConnectReq sockUDP trackerAddr timeWaitSec = do transIdReq <- randomIO
                                                    sendTo sockUDP (toStrictByteString $ execWriter $ do tell $ int64BE connectConnId
                                                                                                         tell $ int32BE connectAction
                                                                                                         tell $ int32BE transIdReq) trackerAddr
                                                    maybeResponse <- timeout (timeWaitSec*1000000) $ recv sockUDP 16
                                                    return $ liftM LC.fromStrict maybeResponse >>= validConnectRes transIdReq

-- Check if the Connect Response received is valid or not
validConnectRes :: Int32 -> LC.ByteString -> Maybe LC.ByteString
validConnectRes transIdReq response = do guard $ LC.length response >= 16
                                         let (actionRes,rest) = LC.splitAt 4 response
                                             (transIdRes,connIdRes) = LC.splitAt 4 rest
                                         guard $ (decode actionRes :: Int32) == connectAction
                                         guard $ (decode transIdRes :: Int32) == transIdReq
                                         return connIdRes

-- Send an announce request to the UDP Tracker
sendAnnounceReq :: LC.ByteString -> SockAddr -> Stateless -> Int -> Torrent -> IO LC.ByteString
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
                                                                       case liftM LC.fromStrict maybeResponse >>= validAnnounceRes transIdReq of
                                                                            Nothing -> return LC.empty
                                                                            Just peerRes -> return peerRes

-- Check if the Announce Response received is valid or not
validAnnounceRes :: Int32 -> LC.ByteString -> Maybe LC.ByteString
validAnnounceRes transIdReq response = do guard $ LC.length response >=20
                                          let (actionRes,rest) = LC.splitAt 4 response
                                              (transIdRes,details) = LC.splitAt 4 rest
                                          guard $ (decode actionRes :: Int32) == announceAction
                                          guard $ (decode transIdRes :: Int32) == transIdReq
                                          return $ LC.drop 12 details

-- Send a connect followed by an announce to a UDP Tracker and then parse the response appropriately to get the PeerList
getPeersUDP :: Tracker -> Stateless -> Torrent -> IO LC.ByteString
getPeersUDP udpTracker constants stateful = do possibleAddr <- try (makeSockAddr udpTracker) :: IO (Either SomeException SockAddr)
                                               case possibleAddr of
                                                 Left _ -> return LC.empty
                                                 Right trackerAddr -> do -- Tracker timeout for each connect set to 30 seconds. Change?
                                                                         maybeConnect <- sendConnectReq (getUDPSocket constants) trackerAddr 30
                                                                         case maybeConnect of Nothing -> return LC.empty
                                                                                              Just connId -> sendAnnounceReq connId trackerAddr constants 30 stateful
