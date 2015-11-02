module HTTPTracker where
import Types
import TypesHelp
import System.IO
import Network.URL
import Network.HTTP
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Language.Haskell.TH.Ppr as L

addParam :: Maybe URL -> (String,String) -> Maybe URL
addParam url (key,val) = case url of
        Just u -> Just $ add_param u (key,val)
        Nothing -> Nothing

formUrl :: String -> [(String,String)] -> Maybe URL
formUrl url [] = importURL url
formUrl "" _ = Nothing
formUrl url param = foldl addParam (importURL url) param

getParam :: Integer -> Integer -> Integer -> Integer -> String -> Integer -> Integer -> [(String,String)]
getParam port up dw left event com num
        | com == 0 = [("port",show port),("uploaded",show up),("downloaded",show dw),("left",show left),("event",event),("numwant",show num)]
        | otherwise = [("port",show port),("uploaded",show up),("downloaded",show dw),("left",show left),("event",event),("compact","1"),("numwant",show num)]

getUrl :: String -> BL.ByteString -> BL.ByteString -> String
getUrl tracker infoHashEsc peerIdEsc = tracker ++ "?" ++ L.bytesToString (BL.unpack infoHashEsc) ++ "&" ++ L.bytesToString (BL.unpack peerIdEsc)

requestUrl :: String -> BL.ByteString -> BL.ByteString -> Integer -> Integer -> Integer -> Integer -> String -> Integer -> Integer -> Maybe URL
requestUrl tr infoHashEsc peerIdEsc port up dw left event com num = z where
        tracker = getUrl tr infoHashEsc peerIdEsc
        param = getParam port up dw left event com num
        z = formUrl tracker param

checkResponse :: URL -> IO (Maybe String)
checkResponse url = do
        response <- simpleHTTP $ getRequest $ exportURL url
        body <- getResponseBody response
        code <- getResponseCode response
        return (if first code == 2 then Just body else Nothing)

first :: (Eq a, Eq b, Eq c) => (a,b,c) -> a
first (a,b,c) = a

eventToString :: Event -> String
eventToString e = case decodeEvent e of 1 -> "completed"
                                        3 -> "stopped"
                                        _ -> "started"

checkResult :: Maybe URL -> IO (Maybe String)
checkResult url = case url of
        Just u -> checkResponse u
        Nothing -> return Nothing

getResponse :: Tracker -> Stateless -> Torrent -> IO (Maybe String)
getResponse httpTracker constants stateful = checkResult url where
                                                        url = requestUrl tracker infoHashEsc peerIdEsc 6881 0 dw left event 1 50
                                                        tracker = getURL httpTracker
                                                        infoHashEsc = getHash (getInfoHash constants)
                                                        peerIdEsc = getHash (getPeerId constants)
                                                        dw = getDownload $ getPieces stateful
                                                        left = getLeft $ getPieces stateful
                                                        event = eventToString $ getEvent stateful
