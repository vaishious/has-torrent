{-# LANGUAGE PackageImports #-}
module HasTorrent.File (
                spaceAvailable,
                createMultipleFiles,
                createSingleFile,
                writePiece,
            ) where
import HasTorrent.Types
import HasTorrent.Types.TypesHelp

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
fullFoldPath torrentPath listPath = ("./"++) $ foldl (\pref suff -> pref ++ "/" ++ suff) torrentPath listPath

-- Create and allocate the entire space for the file
-- Make sure all parent directories are created before calling
createAllocFile :: File -> IO ()
createAllocFile (File filePath fileSize) = do fileFd <- openFd filePath WriteOnly (Just stdFileMode) defaultFileFlags
                                              fileAllocate fileFd 0 (fromIntegral fileSize)
                                              closeFd fileFd

-- Given the file in recursive .torrent format create all it's parent directories and the file itself
createFileWithDir :: FilePath -> [FilePath] -> Integer -> IO ()
createFileWithDir torrentPath listPath fileSize = do createDirectoryIfMissing True $ fullFoldPath torrentPath $ init listPath
                                                     createAllocFile $ File (fullFoldPath torrentPath listPath) fileSize

-- Convert single recursive file from the torrent file to a better data type
toFile :: FilePath -> ([FilePath],Integer) -> File
toFile torrentPath (listPath,size) = File (fullFoldPath torrentPath listPath) size

-- Convert all recursive files to a better FileList data type format
readFileList :: FilePath -> [([FilePath],Integer)] -> FileList
readFileList torrentPath = V.fromList . map (toFile torrentPath)

-- Given all file recursive structures, create all the files, necessary directory structure and return a in formatted FileList data type
-- Used when in Multiple File Mode
createMultipleFiles :: FilePath -> [([FilePath],Integer)] -> IO FileList
createMultipleFiles torrentPath allFiles = do forM_ allFiles $ uncurry (createFileWithDir torrentPath)
                                              return $ readFileList torrentPath allFiles

-- Creates a Single File in current directory
-- Used when in Single File Mode
createSingleFile :: FilePath -> Integer -> IO FileList
createSingleFile filePath fileSize = do let relativePath = "./" ++ filePath
                                        let file = File relativePath fileSize
                                        createAllocFile file
                                        return $ V.singleton file

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
