import Network.Socket
import Data.ByteString.Builder
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString.Char8 as C
import Data.Int
import Data.Byteable
import Data.Monoid
import Data.Word
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B

-- remotePort and ipAddr are those of the peer
makeTCPSock :: Socket -> Int -> Int -> IO Socket
makeTCPSock sockUDP remotePort ipAddr = do bindAddr <- getSocketName sockUDP
                                           sockTCP <- socket AF_INET Stream defaultProtocol
                                           bind sockTCP bindAddr
                                           connect sockTCP $ SockAddrInet (fromIntegral remotePort) $ fromIntegral ipAddr
                                           return sockTCP

initHandshake :: Socket -> Int -> String -> C.ByteString -> C.ByteString -> IO Int
initHandshake sockTCP pStrLen pStr infoHash peerId = do let init = mempty :: Builder
                                                        let final = init `mappend` ((int8 . fromIntegral) pStrLen) `mappend` (string8 pStr) `mappend` (string8 "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL") `mappend` (byteString infoHash) `mappend` (byteString peerId)
                                                        NSB.send sockTCP $ LB.toStrict $ toLazyByteString final
