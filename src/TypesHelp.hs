module TypesHelp where
import Types
import Data.Int
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Lazy as M
import qualified Data.Vector as V
import qualified Data.Set as S

pStr :: String
pStr = "BitTorrent protocol"

pStrLen :: Int8
pStrLen = 19

reservedBytes :: BL.ByteString
reservedBytes = BL.replicate 8 0

-- 16 KB blocks
blockLength :: (Integral a) => a
blockLength = 16384

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

pieceToReqs :: Int -> PieceList -> S.Set RequestId
pieceToReqs index pieces = S.fromList $ map RequestId $ zip3 (repeat index) (fst $ unzip $ M.toList $ getBlocks (pieces V.! index)) (map getLength (snd $ unzip $ M.toList $ getBlocks (pieces V.! index)))

initPeerState :: PeerState
initPeerState = PeerState True False True False
