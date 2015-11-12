module HasTorrent.Tracker.HTTPTracker (
                                        getResponse,
                                      ) where
import HasTorrent.Types
import HasTorrent.Types.TypesHelp

import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Types as T
import Data.Maybe
import Language.Haskell.TH.Ppr
import Network.HTTP
import Network.Socket
import qualified Data.List.Zipper as Z
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Binary

formUrlBase :: String -> String -> String -> String
formUrlBase tracker infoHashEsc peerIdEsc = tracker ++ "?info_hash=" ++ infoHashEsc ++ "&peer_id=" ++ peerIdEsc

formUrl :: String -> [Integer] -> String -> String
formUrl tr (port:up:dw:left:com:[]) event = tr ++ "&port=" ++ show port ++ "&event=" ++ event ++ "&uploaded=" ++ show up ++ "&downloaded=" ++ show dw ++ "&left=" ++ show left ++ "&compact=" ++ show com

requestUrl :: String -> String -> String -> Integer -> Integer -> Integer -> Integer -> String -> Integer -> String
requestUrl tr ih pid port up dw left event com = url where
                                                        base = formUrlBase tr ih pid
                                                        param = port:up:dw:left:com:[]
                                                        url = formUrl base param event

getEscaped :: Stateless -> String
getEscaped constants = bytesToString $ BL.unpack $ BL.fromStrict $ T.urlEncode False (BL.toStrict $ getHash $ getInfoHash constants)

eventToString :: Event -> String
eventToString e = case decodeEvent e of 1 -> "completed"
                                        3 -> "stopped"
                                        _ -> "started"

checkResult :: String -> IO (Maybe String)
checkResult url = do
                response <- simpleHTTP $ getRequest url
                body <- getResponseBody response
                code <- getResponseCode response
                return (if getCodeStatus code then parseOutput body else Nothing)

getIndex :: String -> String -> Int -> Maybe Int
getIndex str xs val
                | z > y = Nothing
                | take z str == xs = Just val
                | otherwise = getIndex (tail str) xs (val + 1) where
                                                                z = length xs
                                                                y = length str

parseOutput :: String -> Maybe String
parseOutput str = if isNothing val
                        then Nothing
                        else trimPeers $ drop (fromJust val) str where
                                                        val = getIndex str "peers" 0

trimPeers :: String -> Maybe String
trimPeers str = if isNothing val
                        then Nothing
                        else Just $ init $ drop (1 + fromJust val) str where
                                val = getIndex str ":" 0

getCodeStatus :: ResponseCode -> Bool
getCodeStatus (a, b, c) = a==2

getResponse :: Tracker -> Stateless -> Torrent -> IO BL.ByteString
getResponse httpTracker constants stateful = do let tracker = getURL httpTracker
                                                let infoHashEsc = getEscaped constants
                                                let peerIdEsc = "-DE13C0-2GUqvokj489s"
                                                let dw = getDownload $ getPieces stateful
                                                let left = getLeft $ getPieces stateful
                                                let event = eventToString $ getEvent stateful
                                                let url = requestUrl tracker infoHashEsc peerIdEsc 6881 0 dw left event 1
                                                resp <- checkResult url
                                                case resp of Nothing -> return LC.empty
                                                             Just peers -> return $ LC.pack peers
