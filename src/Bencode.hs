module Bencode where
import Data.BEncode
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base16.Lazy as BS
import qualified Data.Map as M
import Crypto.Hash.SHA1
import Data.Int
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as C
import Data.Byteable
import Types
import Network.URI
import Data.Char
import Data.Maybe

readAndDecode :: FilePath -> IO (Maybe BEncode)
readAndDecode fp = do bs <- B.readFile fp
                      return (bRead bs)

infoHash :: Maybe BEncode -> Maybe (C.ByteString)
infoHash (Just (BDict be)) = Just $ toBytes $ hashlazy $ bPack infoDict
                           where Just infoDict = M.lookup "info" be
infoHash _ = Nothing

getFileList :: Maybe BEncode -> Maybe BEncode
getFileList = successiveLookup ["info","files"]

getFiles :: BEncode -> [([FilePath],Integer)]
getFiles (BList []) = []
getFiles (BList ((BDict map):xs)) = (pathlist,len) : (getFiles (BList xs))
                                  where (Just (BInt len)) = M.lookup "length" map
                                        (Just (BList list)) = M.lookup "path" map
                                        pathlist = decodePath list

decodePath :: [BEncode] -> [FilePath]
decodePath [] = []
decodePath ((BString x):xs) = (LC.unpack x) : (decodePath xs)

successiveLookup :: [String] -> Maybe BEncode -> Maybe BEncode
successiveLookup [] be = be
successiveLookup (x:xs) (Just (BDict bd)) = successiveLookup xs (M.lookup x bd)
successiveLookup _ _ = Nothing

splitPieceHash :: B.ByteString -> [B.ByteString]
splitPieceHash piecehash = case B.length piecehash > 0 of
                               True -> (head : (splitPieceHash piecehash'))
                                    where (head,piecehash') = B.splitAt 20 piecehash
                               _    -> []

pieceHashList :: Maybe BEncode -> Maybe [B.ByteString]
pieceHashList be = case successiveLookup ["info","pieces"] be of
                        (Just (BString bs)) -> Just $ splitPieceHash bs
                        _                   -> Nothing

getOverallSize :: [([FilePath],Integer)] -> Integer
getOverallSize [] = 0
getOverallSize (x:xs) = (snd x) + (getOverallSize xs)

getPieceLength :: Maybe BEncode -> Maybe Integer
getPieceLength be = case successiveLookup ["info","piece length"] be of
                      (Just (BInt len)) -> Just len
                      _                 -> Nothing

pieceLengthList :: Integer -> Integer -> [Int]
pieceLengthList totalSize pieceLength = take (fromIntegral $ quot totalSize pieceLength) (repeat $ fromIntegral pieceLength) ++ last
                                      where last = if mod totalSize pieceLength == 0 then [] else [fromIntegral $ mod totalSize pieceLength]

pieceInfo :: [Int] -> [B.ByteString] -> PieceInfo
pieceInfo lenList hashList = V.fromList $ zipWith SinglePieceInfo lenList $ map Hash hashList

-- What if input is not of the type (BList a)?
announceURL :: BEncode -> String
announceURL (BList urlList) = LC.unpack url
                         where (BString url) = head urlList

announceList :: Maybe BEncode -> Maybe [String]
announceList be = case successiveLookup ["announce-list"] be of
                          (Just (BList announceList)) -> Just $ map announceURL announceList
                          _                -> Nothing

uriToTracker :: String -> Tracker
uriToTracker uri = case filter isLetter $ uriScheme parsedURI of
                           "http" -> HTTPTracker uri
                           "udp"  -> UDPTracker (uriRegName auth) (fromIntegral $ read $ filter isNumber $ uriPort auth)
                                  where auth = fromJust $ uriAuthority parsedURI
                 where parsedURI = fromJust $ parseURI uri

getTrackerList :: [String] -> TrackerList
getTrackerList = map uriToTracker
