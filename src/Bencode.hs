module Bencode (
                   setStateless,
                   setStateful,
               ) where
import Types
import TypesHelp
import File

import Data.BEncode
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Crypto.Hash.SHA1
import Data.Int
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T
import Data.Text.Encoding
import Network.URI
import Network.Socket
import Data.Char
import Data.Maybe
import Data.List
import Control.Monad (replicateM)
import System.Random
import Data.Word
import qualified Data.Set as S
import System.Random.Shuffle (shuffleM)
import qualified Data.List.Zipper as Z

readAndDecode :: FilePath -> IO (Maybe BEncode)
readAndDecode fp = do bs <- BL.readFile fp
                      return $ bRead bs

findInfoHash :: BEncode -> Maybe BL.ByteString
findInfoHash (BDict be) = fmap (BL.fromStrict . hashlazy . bPack) (M.lookup "info" be)
findInfoHash _          = Nothing

readFileDict :: BEncode -> Maybe BEncode
readFileDict = successiveLookup ["info","files"]

getFiles :: BEncode -> [([FilePath],Integer)]
getFiles (BList (BDict map:xs)) = (pathlist,len) : getFiles (BList xs)
                                where (Just (BInt len))   = M.lookup "length" map
                                      (Just (BList list)) = M.lookup "path" map
                                      pathlist            = decodePath list
getFiles _                      = []

extractString :: BEncode -> String
extractString (BString str) = T.unpack $ decodeUtf8 $ LC.toStrict str
extractString _ = ""

getRootPath :: BEncode -> Maybe FilePath
getRootPath be = fmap extractString (successiveLookup ["info","name"] be)

decodePath :: [BEncode] -> [FilePath]
decodePath = map extractString

successiveLookup :: [String] -> BEncode -> Maybe BEncode
successiveLookup [] be = Just be
successiveLookup (x:xs) (BDict bd) = maybe Nothing (successiveLookup xs) (M.lookup x bd)
successiveLookup _ _ = Nothing

splitPieceHash :: BL.ByteString -> [BL.ByteString]
splitPieceHash piecehash = case BL.length piecehash > 0 of
                               True -> head : splitPieceHash piecehash'
                                    where (head,piecehash') = BL.splitAt (fromIntegral lenHash) piecehash
                               _    -> []

pieceHashList :: BEncode -> Maybe [BL.ByteString]
pieceHashList be = case successiveLookup ["info","pieces"] be of
                        (Just (BString bs)) -> Just $ splitPieceHash bs
                        _                   -> Nothing

getOverallSize :: FileList -> Integer
getOverallSize = V.foldr ((+) . getSize) 0

readPieceLength :: BEncode -> Maybe Integer
readPieceLength be = case successiveLookup ["info","piece length"] be of
                      (Just (BInt len)) -> Just len
                      _                 -> Nothing

pieceLengthList :: Integer -> Integer -> [Int]
pieceLengthList totalSize pieceLength = replicate (fromIntegral $ quot totalSize pieceLength) (fromIntegral pieceLength) ++ last
                                      where last = if mod totalSize pieceLength == 0 then [] else [fromIntegral $ mod totalSize pieceLength]

setPieceInfo :: [Int] -> [BL.ByteString] -> FileList -> PieceInfo
setPieceInfo lenList hashList fileList = V.fromList $ zipWith3 SinglePieceInfo lenList (map Hash hashList) (coveredFileList fileList (map fromIntegral lenList))

-- What if input is not of the type (BList a)?
announceURL :: BEncode -> [String]
announceURL (BList urlList) = map extractString urlList

announceList :: BEncode -> Maybe [String]
announceList be = case successiveLookup ["announce-list"] be of
                          (Just (BList announceList)) -> Just $ concatMap announceURL announceList
                          _                           -> Nothing

-- Handle HTTPS?
-- Test for isJust?
uriToTracker :: String -> Tracker
uriToTracker uri = case filter isLetter $ uriScheme parsedURI of
                           "http" -> HTTPTracker uri
                           "udp"  -> let port = uriPort auth
                                     in if null port then UDPTracker (uriRegName auth) 443
                                         else UDPTracker (uriRegName auth) (fromIntegral $ read $ filter isNumber $ uriPort auth)
                                  where auth = fromJust $ uriAuthority parsedURI
                 where parsedURI = fromJust $ parseURI uri

readTrackerList :: [String] -> TrackerList
readTrackerList = map uriToTracker

extractTrackers :: BEncode -> Maybe TrackerList
extractTrackers be = case announceList be
                        of Just uriList -> Just $ readTrackerList uriList
                           Nothing -> if isJust (successiveLookup ["announce"] be)
                                      then Just [uriToTracker $ extractString $ fromJust $ successiveLookup ["announce"] be]
                                      else Nothing

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

genPeerID :: IO BL.ByteString
genPeerID = do let randomWord8 = getStdRandom random :: IO Word8
               word8List <- replicateM lenHash randomWord8
               return $ BL.pack word8List

makeUDPSock :: IO Socket
makeUDPSock = do sock <- socket AF_INET Datagram defaultProtocol
                 setSocketOption sock ReuseAddr 1
                 setSocketOption sock ReusePort 1
                 bind sock (SockAddrInet aNY_PORT iNADDR_ANY)
                 return sock

listeningTCP :: PortNumber -> IO Socket
listeningTCP udpPort = do sock <- socket AF_INET Stream defaultProtocol
                          setSocketOption sock ReuseAddr 1
                          setSocketOption sock ReusePort 1
                          bind sock (SockAddrInet udpPort iNADDR_ANY)
                          listen sock 2
                          return sock

performChecks :: Maybe BEncode -> Bool
performChecks (Just be) = all ($ be) [
                                         isJust . getRootPath,
                                         isJust . readFileDict,
                                         isJust . findInfoHash,
                                         isJust . extractTrackers,
                                         isJust . readPieceLength,
                                         isJust . pieceHashList
                                     ]
performChecks _         = False

setStateless :: FilePath -> IO (Maybe Stateless)
setStateless torrentFile = do maybeBE             <- readAndDecode torrentFile
                              if performChecks maybeBE
                              then do let be = fromJust maybeBE
                                      peerID         <- genPeerID
                                      udpSocket      <- makeUDPSock
                                      port           <- socketPort udpSocket
                                      tcpSocket      <- listeningTCP port
                                      let rootPath    = fromJust $ getRootPath be
                                      fileList       <- createAllFiles rootPath (getFiles (fromJust $ readFileDict be))
                                      let infoHash    = Hash $ fromJust $ findInfoHash be
                                      let trackerList = fromJust $ extractTrackers be
                                      let pieceLenList = pieceLengthList (getOverallSize fileList) (fromJust $ readPieceLength be)
                                      let pieceInfo   = setPieceInfo pieceLenList (fromJust $ pieceHashList be) fileList
                                      return $ Just $ Stateless infoHash pieceInfo (Hash peerID) trackerList fileList tcpSocket udpSocket
                              else return Nothing

randomPerm :: (Integral a) => a -> IO [a]
randomPerm numPieces = shuffleM [0..(numPieces-1)]

setBlocks :: Int -> Int -> [(Int,Block)]
setBlocks pieceLength pieceOffset = if pieceLength > blockLength
                                    then (pieceOffset,Block False BL.empty pieceOffset blockLength):setBlocks (pieceLength - blockLength) (pieceOffset+blockLength)
                                    else [(pieceOffset,Block False BL.empty pieceOffset pieceLength)]

setPiece :: SinglePieceInfo -> Piece
setPiece SinglePieceInfo { getPieceLength = pieceLen } = Piece False $ M.fromList (setBlocks pieceLen 0)

setPieceList :: PieceInfo -> PieceList
setPieceList = V.map setPiece

setStateful :: Stateless -> IO Torrent
setStateful constants = do let pieces = setPieceList $ getPieceInfo constants
                           pieceOrd <- randomPerm $ V.length pieces
                           return $ Torrent Started pieces pieceOrd S.empty Nothing Z.empty Z.empty
