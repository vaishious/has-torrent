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
import Data.List
import File
import Control.Monad (replicateM)
import System.Random
import Data.Word

readAndDecode :: FilePath -> IO (Maybe BEncode)
readAndDecode fp = do bs <- B.readFile fp
                      return $ bRead bs

infoHash :: Maybe BEncode -> Maybe B.ByteString
infoHash (Just (BDict be)) = Just $ B.fromStrict $ toBytes $ hashlazy $ bPack infoDict
                           where Just infoDict = M.lookup "info" be
infoHash _ = Nothing

getFileList :: Maybe BEncode -> Maybe BEncode
getFileList = successiveLookup ["info","files"]

getFiles :: BEncode -> [([FilePath],Integer)]
getFiles (BList (BDict map:xs)) = (pathlist,len) : getFiles (BList xs)
                                where (Just (BInt len))   = M.lookup "length" map
                                      (Just (BList list)) = M.lookup "path" map
                                      pathlist            = decodePath list
getFiles _                      = []

decodePath :: [BEncode] -> [FilePath]
decodePath [] = []
decodePath (BString x:xs) = LC.unpack x : decodePath xs

successiveLookup :: [String] -> Maybe BEncode -> Maybe BEncode
successiveLookup [] be = be
successiveLookup (x:xs) (Just (BDict bd)) = successiveLookup xs (M.lookup x bd)
successiveLookup _ _ = Nothing

splitPieceHash :: B.ByteString -> [B.ByteString]
splitPieceHash piecehash = case B.length piecehash > 0 of
                               True -> head : splitPieceHash piecehash'
                                    where (head,piecehash') = B.splitAt 20 piecehash
                               _    -> []

pieceHashList :: Maybe BEncode -> Maybe [B.ByteString]
pieceHashList be = case successiveLookup ["info","pieces"] be of
                        (Just (BString bs)) -> Just $ splitPieceHash bs
                        _                   -> Nothing

getOverallSize :: FileList -> Integer
getOverallSize = V.foldr ((+) . getSize) 0

getPieceLength :: Maybe BEncode -> Maybe Integer
getPieceLength be = case successiveLookup ["info","piece length"] be of
                      (Just (BInt len)) -> Just len
                      _                 -> Nothing

pieceLengthList :: Integer -> Integer -> [Int]
pieceLengthList totalSize pieceLength = replicate (fromIntegral $ quot totalSize pieceLength) (fromIntegral pieceLength) ++ last
                                      where last = if mod totalSize pieceLength == 0 then [] else [fromIntegral $ mod totalSize pieceLength]

pieceInfo :: [Int] -> [B.ByteString] -> FileList -> PieceInfo
pieceInfo lenList hashList fileList = V.fromList $ zipWith3 SinglePieceInfo lenList (map Hash hashList) (coveredFileList fileList (map fromIntegral (0:lenList)))

-- What if input is not of the type (BList a)?
announceURL :: BEncode -> String
announceURL (BList urlList) = LC.unpack url
                         where (BString url) = head urlList

announceList :: Maybe BEncode -> Maybe [String]
announceList be = case successiveLookup ["announce-list"] be of
                          (Just (BList announceList)) -> Just $ map announceURL announceList
                          _                           -> Nothing

uriToTracker :: String -> Tracker
uriToTracker uri = case filter isLetter $ uriScheme parsedURI of
                           "http" -> HTTPTracker uri
                           "udp"  -> UDPTracker (uriRegName auth) (fromIntegral $ read $ filter isNumber $ uriPort auth)
                                  where auth = fromJust $ uriAuthority parsedURI
                 where parsedURI = fromJust $ parseURI uri

getTrackerList :: [String] -> TrackerList
getTrackerList = map uriToTracker

runningSum :: [Integer] -> [Integer]
runningSum = scanl1 (+)

-- Makes list of endpoints lying in either filePositionList or piecePositionList
rmdups :: [Integer] -> [Integer] -> [Integer]
rmdups filePositionList piecePositionList = map head . group . sort $ filePositionList ++ piecePositionList

-- For each piece, finds all the file endpoints lying between its endpoints
piecewiseSplit :: [Integer] -> [Integer] -> [[Integer]]
piecewiseSplit _ [] = []
piecewiseSplit allPositionList (y:ys) = le : piecewiseSplit (y:g) ys
                                      where (le,g) = span (<=y) allPositionList

-- For each file a piece covers, this makes a list of tuples of (file index,offset in file,length in file)
filewiseSplit :: [Integer] -> [Integer] -> [(Integer,Integer,Integer)]
filewiseSplit filePositionList (x:y:xs) = if not (null le)
                                          then (fromIntegral $ length le - 1 , x - last le , y - x): filewiseSplit filePositionList (y:xs)
                                          else filewiseSplit filePositionList (y:xs)
                                        where le = takeWhile (<=x) filePositionList
filewiseSplit _ _                       = []

allSplit :: [Integer] -> [[Integer]] -> [[(Integer,Integer,Integer)]]
allSplit filePositionList = map (filewiseSplit filePositionList)

coveredFileList :: FileList -> [Integer] -> [CoveredFileList]
coveredFileList fileList pieceLengthList = map g $ allSplit filePositionList $ piecewiseSplit (rmdups filePositionList piecePositionList) piecePositionList
                                         where filePositionList   = (:) 0 $ runningSum $ map getSize (V.toList fileList)
                                               piecePositionList  = runningSum pieceLengthList
                                               f (a,b,c)          = CoveredFile (getFilePath $ (V.!) fileList (fromIntegral a)) b (fromIntegral c)
                                               g a                = V.fromList $ map f a

toFile :: FilePath -> ([FilePath],Integer) -> File
toFile rootPath (listPath,size) = File (fullFoldPath rootPath listPath) size

fileList :: FilePath -> [([FilePath],Integer)] -> FileList
fileList rootPath files = V.fromList $ map (toFile rootPath) files

genPeerID :: IO B.ByteString
genPeerID = do let randomWord8 = getStdRandom random :: IO Word8
               word8List <- replicateM 20 randomWord8
               return $ B.pack word8List

setStateless :: FilePath -> FilePath -> IO (Maybe Stateless)
setStateless torrentFile rootPath = do be             <- readAndDecode torrentFile
                                       peerid         <- genPeerID
                                       let infohash    = Hash $ fromJust $ infoHash be
                                       let trackerList = getTrackerList $ fromJust $ announceList be
                                       let filelist    = fileList rootPath (getFiles (fromJust (Bencode.getFileList be)))
                                       let pieceinfo   = pieceInfo (pieceLengthList (getOverallSize filelist) (fromJust $ Bencode.getPieceLength be)) (fromJust $ pieceHashList be) filelist
                                       return $ Just $ Stateless infohash pieceinfo (Hash peerid) trackerList filelist
