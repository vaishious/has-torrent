module File where
import System.IO
import System.Directory
import System.Posix.IO
import System.Posix.Fcntl
import System.Posix.Files
import System.DiskSpace
import Data.Int
import qualified Data.Map as Map
import Data.BEncode
import Control.Monad

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
