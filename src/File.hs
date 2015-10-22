{-# LANGUAGE PackageImports #-}
module File where
import Types
import TypesHelp
import System.IO
import System.Directory
import System.Posix.IO
import System.Posix.Fcntl
import System.Posix.Files
import "unix-bytestring" System.Posix.IO.ByteString
import System.Posix.Types
import System.DiskSpace
import Data.Int
import qualified Data.Map as Map
import Data.BEncode
import qualified Data.Vector as V
import Control.Monad
import qualified Data.ByteString.Lazy as BL

-- Gives us how much space is available in the current working directory. Directory must be set to the user download directory
spaceAvailable :: IO Integer
spaceAvailable = getAvailSpace "./"

fullFoldPath :: FilePath -> [FilePath] -> FilePath
fullFoldPath rootPath listPath = ("./"++) $ foldl (\pref suff -> pref ++ "/" ++ suff) rootPath listPath

createAllocFile :: FilePath -> Integer -> IO ()
createAllocFile filePath fileSize = do fileFd <- openFd filePath WriteOnly (Just stdFileMode) defaultFileFlags
                                       fileAllocate fileFd 0 (fromIntegral fileSize)
                                       closeFd fileFd

createFileWithDir :: FilePath -> [FilePath] -> Integer -> IO ()
createFileWithDir rootPath listPath fileSize = do createDirectoryIfMissing True $ fullFoldPath rootPath $ init listPath
                                                  createAllocFile (fullFoldPath rootPath listPath) fileSize

createAllFiles :: FilePath -> [([FilePath],Integer)] -> IO ()
createAllFiles rootPath allFiles = forM_ allFiles (\x -> createFileWithDir rootPath (fst x) (snd x))

splitWrite :: BL.ByteString -> CoveredFile -> IO ByteCount
splitWrite pieceData cFile = do fd <- openFd (getCoveredFilePath cFile) WriteOnly Nothing defaultFileFlags
                                let off = fromIntegral $ getCoveredFileOffset cFile
                                let len = fromIntegral $ getCoveredFileLength cFile
                                let (_,right) = BL.splitAt off pieceData
                                let (left,_)  = BL.splitAt (off + len) right
                                fdPwrite fd (BL.toStrict left) (COff off)

writePiece :: Int -> Stateless -> Torrent -> IO ()
writePiece index constants torrent = mapM_ (splitWrite $ getPieceData $ getPieces torrent V.! index) (V.toList $ getCoveredFileList $ getPieceInfo constants V.! index)
