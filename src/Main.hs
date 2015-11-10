import HasTorrent
import PeerAlgorithm

import Data.Maybe
import Control.Monad
import Control.Monad.State
import System.Environment
import System.IO
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import Network.Socket (getSocketName)

-- We need to check if sufficient space is available
main :: IO ()
main = do fh <- fileHandler "has-torrent.log" DEBUG >>= \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg\n")
          sh <- streamHandler stderr DEBUG >>= \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg\n")
          updateGlobalLogger rootLoggerName removeHandler
          updateGlobalLogger "HasTorrent" (setHandlers [fh, sh])
          updateGlobalLogger "HasTorrent" (setLevel INFO)
          argList <- getArgs
          let torrentFile = head argList
          maybeConstants <- setStateless torrentFile
          let constants = fromJust maybeConstants

          sockName <- getSocketName $ getTCPSocket constants
          infoM "HasTorrent" $ "Socket Name: "++ show sockName

          torrent <- setStateful constants
          execStateT (startTorrent constants) torrent
          return ()

startTorrent :: Stateless -> StateT Torrent IO ()
startTorrent constants = do
                            findAndAddPeers constants
                            download constants
