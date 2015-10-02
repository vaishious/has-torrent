import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.BSD
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Builder
import Data.Binary
import Data.Int
import Data.Word
import Data.Monoid
import Data.Maybe
import System.Timeout
import System.Random
import Control.Monad

makeSockAddr :: String -> Int -> IO SockAddr
makeSockAddr trackerName trackerPort = do tracker <- getHostByName trackerName
                                          return $ SockAddrInet (fromIntegral trackerPort) (hostAddress tracker)

makeSocket :: IO Socket
makeSocket = socket AF_INET Datagram 17

sockConnTracker :: String -> Int -> IO Socket
sockConnTracker trackerName trackerPort = do trackerSock <- makeSockAddr trackerName trackerPort
                                             sockUDP <- makeSocket
                                             connect sockUDP trackerSock
                                             return sockUDP

connectConnId :: Int64
connectConnId = 4497486125440

connectAction :: Int32
connectAction = 0

genRandInt32 :: IO Int32
genRandInt32 = randomIO

sendConnectRequest :: Int -> Socket -> IO (Maybe Int64)
sendConnectRequest timeWait sockUDP = do transactionId <- genRandInt32
                                         send sockUDP $ BL.toStrict $ toLazyByteString $ (int64BE connectConnId) `mappend` (int32BE connectAction) `mappend` (int32BE transactionId)
                                         maybeResponse <- timeout (timeWait*1000000) (recv sockUDP 16)
                                         print maybeResponse
                                         return $ liftM BL.fromStrict maybeResponse >>= (parseConnectResponse transactionId)

parseConnectResponse :: Int32 -> BL.ByteString -> (Maybe Int64)
parseConnectResponse transactionId response = if BL.length response < 16
                                                 then Nothing
                                                 else let (responseAction,responseTransConn) = BL.splitAt 4 response
                                                      in if (decode responseAction :: Int32) /= connectAction
                                                            then Nothing
                                                            else let (responseTrans,responseConn) = BL.splitAt 4 responseTransConn
                                                                 in if (decode responseTrans :: Int32) /= transactionId
                                                                       then Nothing
                                                                       else Just (decode responseConn :: Int64)
