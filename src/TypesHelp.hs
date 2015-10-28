module TypesHelp where
import Types
import Data.Int
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Set as S

decodeEvent :: (Num a) => Event -> a
decodeEvent None = 0
decodeEvent Completed = 1
decodeEvent Started = 2
decodeEvent Stopped = 3

getPieceDownload :: Piece -> Int
getPieceDownload (Piece _ blocks) = V.foldl (\a b -> if getDownloadStatus b then a + getLength b else a) 0 blocks

getDownload :: PieceList -> Int64
getDownload = V.foldl (\a p -> a + fromIntegral (getPieceDownload p)) 0

getPieceLeft :: Piece -> Int
getPieceLeft (Piece _ blocks) = V.foldl (\a b -> if getDownloadStatus b then a else a + getLength b) 0 blocks

getLeft :: PieceList -> Int64
getLeft = V.foldl (\a p -> a + fromIntegral (getPieceLeft p)) 0

getPieceData :: Piece -> BL.ByteString
getPieceData = BL.concat . map getData . V.toList . getBlocks

minActiveBlocks :: Integral a => a
minActiveBlocks = 100

requestIdToBlock :: RequestId -> PieceList -> Block
requestIdToBlock (RequestId (pieceId,blockId)) = (V.!blockId) . getBlocks . (V.!pieceId)

pieceToReqs :: Int -> PieceList -> S.Set RequestId
pieceToReqs index pieces = S.fromList $ map RequestId $ zip (repeat index) [0..(V.length pieces -1)]
