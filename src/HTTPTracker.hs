import System.IO
import Network.URL
import Network.HTTP
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.UTF8 as U

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

getUrl :: String -> U.ByteString -> U.ByteString -> String
getUrl tracker infoHashEsc peerIdEsc = tracker ++ "?" ++ U.toString infoHashEsc ++ "&" ++ U.toString peerIdEsc

requestUrl :: String -> U.ByteString -> U.ByteString -> Integer -> Integer -> Integer -> Integer -> String -> Integer -> Integer -> Maybe URL
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

getResponse :: Maybe URL -> IO (Maybe String)
getResponse u = case u of
        Just url -> checkResponse url
        Nothing -> return Nothing

--main will have the form of getResponse $ requestUrl input
