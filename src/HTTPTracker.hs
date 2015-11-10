module HTTPTracker where
import Types
import TypesHelp
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Types as T
import Data.Maybe
import Language.Haskell.TH.Ppr
import Network.HTTP

formUrlBase :: String -> String -> String -> String
formUrlBase tracker infoHashEsc peerIdEsc = tracker ++ "?info_hash=" ++ infoHashEsc ++ "&peer_id=" ++ peerIdEsc

formUrl :: String -> [Integer] -> String -> String
formUrl tr (port:up:dw:left:com:[]) event = tr ++ "&port=" ++ show port ++ "&event=" ++ event ++ "&uploaded=" ++ show up ++ "&downloaded=" ++ show dw ++ "&left=" ++ show left ++ "&compact=" ++ show com

requestUrl :: String -> String -> String -> Integer -> Integer -> Integer -> Integer -> String -> Integer -> String
requestUrl tr ih pid port up dw left event com = url where
							base = formUrlBase tr ih pid
							param = port:up:dw:left:com:[]
							url = formUrl base param event

getEscaped :: Stateless -> Int -> String
getEscaped constants c = case c of 
			1 -> bytesToString $ BL.unpack $ BL.fromStrict $ T.urlEncode False (BL.toStrict $ getHash $ getInfoHash constants)
			2 -> bytesToString $ BL.unpack $ BL.fromStrict $ T.urlEncode False (BL.toStrict $ getHash $ getPeerId constants)

eventToString :: Event -> String
eventToString e = case decodeEvent e of 1 -> "completed"
                                        3 -> "stopped"
                                        _ -> "started"

checkResult :: String -> IO (Maybe String)
checkResult url = do
		response <- simpleHTTP $ getRequest url
		body <- getResponseBody response
		code <- getResponseCode response
		return (if getCodeStatus code then Just body else Nothing)

getCodeStatus :: ResponseCode -> Bool
getCodeStatus (a, b, c) = if a == 2 then True else False

getResponse :: Tracker -> Stateless -> Torrent -> IO (Maybe String)
getResponse httpTracker constants stateful = checkResult url where
								url = requestUrl tracker infoHashEsc peerIdEsc 6881 0 dw left event 1
								tracker = getURL httpTracker
								infoHashEsc = getEscaped constants 1
								peerIdEsc = getEscaped constants 2
								dw = getDownload $ getPieces stateful
								left = getLeft $ getPieces stateful
								event = eventToString $ getEvent stateful
