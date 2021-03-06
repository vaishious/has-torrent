module HasTorrent.Types.TypesHelp where
import HasTorrent.Types

import Data.Int
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Lazy as M
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.List.Zipper as Z
import Crypto.Hash.SHA1 (hashlazy)

lenHash :: (Integral a) => a
lenHash = 20

requestTime :: (Num a) => a
requestTime = 120

responseTime :: (Num a) => a
responseTime = 240

effResponseTime :: (Num a) => a
effResponseTime = 90

pStr :: String
pStr = "BitTorrent protocol"

pStrLen :: (Integral a) => a
pStrLen = 19

reservedBytes :: BL.ByteString
reservedBytes = BL.replicate 8 0

-- 16 KB blocks
blockLength :: (Integral a) => a
blockLength = 16384

runningSum :: (Integral a) => [a] -> [a]
runningSum = scanl1 (+)

decodeEvent :: (Integral a) => Event -> a
decodeEvent None = 0
decodeEvent Completed = 1
decodeEvent Started = 2
decodeEvent Stopped = 3

getPieceDownload :: (Integral a) => Piece -> a
getPieceDownload (Piece _ blocks) = M.foldl (\a b -> if getDownloadStatus b then a + fromIntegral (getLength b) else a) 0 blocks

getDownload :: (Integral a) => PieceList -> a
getDownload = V.foldl (\a p -> a + getPieceDownload p) 0

getPieceLeft :: (Integral a) => Piece -> a
getPieceLeft (Piece _ blocks) = M.foldl (\a b -> if getDownloadStatus b then a else a + fromIntegral (getLength b)) 0 blocks

getLeft :: (Integral a) => PieceList -> a
getLeft = V.foldl (\a p -> a + getPieceLeft p) 0

getPieceData :: Piece -> BL.ByteString
getPieceData = BL.concat . map getData . snd . unzip . M.toList . getBlocks

minActiveBlocks :: Integral a => a
minActiveBlocks = 100

minPeerRequests :: Integral a => a
minPeerRequests = 2

maxActivePeers :: Integral a => a
maxActivePeers = 30

pieceToReqs :: Int -> PieceList -> S.Set RequestId
pieceToReqs index pieces = S.fromList $ map RequestId $ zip3 (repeat index) (fst $ unzip $ M.toList $ getBlocks (pieces V.! index)) (map getLength (snd $ unzip $ M.toList $ getBlocks (pieces V.! index)))

toReqMsg :: RequestId -> Message
toReqMsg (RequestId (index,offset,length)) = RequestMsg index offset length

fromReqMsg :: Message -> RequestId
fromReqMsg (RequestMsg index offset length) = RequestId (index,offset,length)
fromReqMsg _ = RequestId (0,0,0)

toCanMsg :: RequestId -> Message
toCanMsg (RequestId (index,offset,length)) = CancelMsg index offset length

fromCanMsg :: Message -> RequestId
fromCanMsg (CancelMsg index offset length) = RequestId (index,offset,length)
fromCanMsg _ = RequestId (0,0,0)

initPeerState :: PeerState
initPeerState = PeerState True False True False

zipLength :: Z.Zipper a -> Int
zipLength (Z.Zip x y) = length x + length y

expectedHash :: Stateless -> Int -> BL.ByteString
expectedHash constants index = getHash $ getPieceHash $ getPieceInfo constants V.! index

computedHash :: Torrent -> Int -> BL.ByteString
computedHash torrent index = BL.fromStrict $ hashlazy $ getPieceData $ getPieces torrent V.! index

eraseBlockData :: Block -> Block
eraseBlockData block = block{ getDownloadStatus = False, getData = BL.empty }

erasePieceData :: Piece -> Piece
erasePieceData piece = piece{ getBlocks = M.map eraseBlockData $ getBlocks piece }

setVerifiedStatus :: Piece -> Piece
setVerifiedStatus piece = piece{ getVerifiedStatus = True }
