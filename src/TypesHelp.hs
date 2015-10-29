module TypesHelp where
import Types
import Data.Int
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Set as S

decodeEvent :: (Integral a) => Event -> a
decodeEvent None = 0
decodeEvent Completed = 1
decodeEvent Started = 2
decodeEvent Stopped = 3

getPieceDownload :: (Integral a) => Piece -> a
getPieceDownload (Piece _ blocks) = V.foldl (\a b -> if getDownloadStatus b then a + fromIntegral (getLength b) else a) 0 blocks

getDownload :: (Integral a) => PieceList -> a
getDownload = V.foldl (\a p -> a + getPieceDownload p) 0

getPieceLeft :: (Integral a) => Piece -> a
getPieceLeft (Piece _ blocks) = V.foldl (\a b -> if getDownloadStatus b then a else a + fromIntegral (getLength b)) 0 blocks

getLeft :: (Integral a) => PieceList -> a
getLeft = V.foldl (\a p -> a + getPieceLeft p) 0

getPieceData :: Piece -> BL.ByteString
getPieceData = BL.concat . map getData . V.toList . getBlocks

minActiveBlocks :: Integral a => a
minActiveBlocks = 100

requestIdToBlock :: RequestId -> PieceList -> Block
requestIdToBlock (RequestId (pieceId,blockId)) = (V.!blockId) . getBlocks . (V.!pieceId)

pieceToReqs :: Int -> PieceList -> S.Set RequestId
pieceToReqs index pieces = S.fromList $ map RequestId $ zip (repeat index) [0..(V.length pieces -1)]
