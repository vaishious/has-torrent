import Bencode
import PeerProtocol
import Tracker
import UDPTracker
import Types
import TypesHelp

import Data.Maybe
import Control.Monad
import System.Environment
import System.IO
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

-- We need to check if sufficient space is available
main :: IO ()
main = do fh <- fileHandler "has-torrent.log" DEBUG >>= \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
          sh <- streamHandler stderr DEBUG >>= \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
          updateGlobalLogger "HasTorrent" (setHandlers [fh, sh])
          updateGlobalLogger "HasTorrent" (setLevel INFO)
