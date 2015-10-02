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

pieceList :: Int64 -> B.ByteString -> [B.ByteString]
pieceList n bs = (head : (pieceList n bs'))
               where (head,bs') = B.splitAt n bs

pieceVector :: B.ByteString -> V.Vector B.ByteString
pieceVector bs = V.fromList $ pieceList 20 bs

readAndDecode :: FilePath -> IO (Maybe BEncode)
readAndDecode fp = do bs <- B.readFile fp
                      return (bRead bs)

infoHash :: Maybe BEncode -> Maybe (C.ByteString)
infoHash (Just (BDict be)) = Just $ toBytes $ hashlazy $ bPack infoDict
                           where Just infoDict = M.lookup "info" be
infoHash _ = Nothing

getFileList :: Maybe BEncode -> Maybe BEncode
getFileList = successiveLookup ["info","files"]

getPiecesHash :: Maybe BEncode -> Maybe BEncode
getPiecesHash = successiveLookup ["info","pieces"]

successiveLookup :: [String] -> Maybe BEncode -> Maybe BEncode
successiveLookup [] be = be
successiveLookup (x:xs) (Just (BDict bd)) = successiveLookup xs (M.lookup x bd)
successiveLookup _ _ = Nothing

getFiles :: BEncode -> [([FilePath],Integer)]
getFiles (BList []) = []
getFiles (BList ((BDict map):xs)) = (pathlist,len) : (getFiles (BList xs))
                                  where (Just (BInt len)) = M.lookup "length" map
                                        (Just (BList list)) = M.lookup "path" map
                                        pathlist = decodePath list

decodePath :: [BEncode] -> [FilePath]
decodePath [] = []
decodePath ((BString x):xs) = (LC.unpack x) : (decodePath xs)
