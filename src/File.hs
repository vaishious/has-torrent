{-# LANGUAGE PackageImports #-}
module File (
                spaceAvailable,
                createAllFiles,
                writePiece,
            ) where
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

-- Gives us how much space is available in the current working directory
spaceAvailable :: IO Integer
spaceAvailable = getAvailSpace "./"

-- Get relative paths of files from the recursive structure specified in the .torrent file
fullFoldPath :: FilePath -> [FilePath] -> FilePath
fullFoldPath rootPath listPath = ("./"++) $ foldl (\pref suff -> pref ++ "/" ++ suff) rootPath listPath

-- Create and allocate the entire space for the file
-- Make sure all parent directories are created before calling
createAllocFile :: File -> IO ()
createAllocFile (File filePath fileSize) = do fileFd <- openFd filePath WriteOnly (Just stdFileMode) defaultFileFlags
                                              fileAllocate fileFd 0 (fromIntegral fileSize)
                                              closeFd fileFd

-- Given the file in recursive .torrent format create all it's parent directories and the file itself
createFileWithDir :: FilePath -> [FilePath] -> Integer -> IO ()
createFileWithDir rootPath listPath fileSize = do createDirectoryIfMissing True $ fullFoldPath rootPath $ init listPath
                                                  createAllocFile $ File (fullFoldPath rootPath listPath) fileSize

-- Convert the recursive file from the torrent file to a better data type
toFile :: FilePath -> ([FilePath],Integer) -> File
toFile rootPath (listPath,size) = File (fullFoldPath rootPath listPath) size

-- Convert all files to a better FileList data type format
readFileList :: FilePath -> [([FilePath],Integer)] -> FileList
readFileList rootPath files = V.fromList $ map (toFile rootPath) files

-- Given all file recursive structures, create all the files, necessary directory structure and return a in formatted FileList data type
createAllFiles :: FilePath -> [([FilePath],Integer)] -> IO FileList
createAllFiles rootPath allFiles = do forM_ allFiles $ uncurry (createFileWithDir rootPath)
                                      return $ readFileList rootPath allFiles


splitWrite :: BL.ByteString -> [CoveredFile] -> IO ()
splitWrite pieceData (CoveredFile fpath off len:xs) = do fd <- openFd fpath WriteOnly Nothing defaultFileFlags
                                                         let (cFileData,restData)  = BL.splitAt (fromIntegral len) pieceData
                                                         fdPwrite fd (BL.toStrict cFileData) $ COff $ fromIntegral off
                                                         closeFd fd
                                                         splitWrite restData xs
splitWrite _ [] = return ()

-- Writes a piece to the respective files it covers
-- Called after the checksum of the piece has been verified
writePiece :: Int -> Stateless -> Torrent -> IO ()
writePiece index constants torrent = splitWrite (getPieceData $ getPieces torrent V.! index) $ V.toList $ getCoveredFileList $ getPieceInfo constants V.! index
