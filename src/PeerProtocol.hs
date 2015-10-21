module PeerProtocol where
import Types
import Control.Monad.Writer
import Data.ByteString.Builder
import Data.Int
import Data.Word
import qualified Data.ByteString.Lazy as BL
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

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
initHandshake sockTCP constants = send sockTCP $ BL.toStrict $ toLazyByteString $ execWriter $ do tell $ int8 pStrLen
                                                                                                  tell $ string8 pStr
                                                                                                  tell $ lazyByteString reservedBytes
                                                                                                  tell $ lazyByteString $ getHash $ getInfoHash constants
                                                                                                  tell $ lazyByteString $ getHash $ getPeerId constants
