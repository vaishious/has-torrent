module Communicate (
                       parseHandshake,
                       peerMessages,
                       msgToByteStr,
                   )where
import Types
import TypesHelp
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Builder
import Data.Maybe (fromJust,isNothing)
import Data.Word
import qualified Data.Binary as BY

-- State Computation on a Lazy ByteString which reads one byte as an Int
readOneByte :: State BL.ByteString Int
readOneByte = state $ \bs -> let (left,right) = BL.splitAt 1 bs
                                 word8 = BY.decode left :: Word8
                             in (fromIntegral word8,right)

-- State Computation on a Lazy ByteString which reads two bytes as an Int
readTwoBytes :: State BL.ByteString Int
readTwoBytes = state $ \bs -> let (left,right) = BL.splitAt 2 bs
                                  word16 = BY.decode left :: Word16
                              in (fromIntegral word16,right)

-- State Computation on a Lazy ByteString which reads four bytes as an Int
readFourBytes :: State BL.ByteString Int
readFourBytes = state $ \bs -> let (left,right) = BL.splitAt 4 bs
                                   word32 = BY.decode left :: Word32
                               in (fromIntegral word32,right)

-- Reads an Int(length) and gives a state computation on a Lazy ByteString which computes and returns the first length bytes of the ByteString
statefulSplit :: Int -> State BL.ByteString BL.ByteString
statefulSplit index = state $ \bs -> BL.splitAt (fromIntegral index) bs

-- Parse a stream (Lazy ByteString) to a single Message if possible
-- If parse fails then we do not delete anything from the stream. This ensures that it will be possible sometime in the future
convert :: State BL.ByteString (Maybe Message)
convert = do unparsed <- get
             let message = unparsed
             if BL.length message > 3
             then do len <- readFourBytes
                     case len of
                          0 -> return $ Just KeepAliveMsg
                          _ -> do message <- get
                                  if len <= fromIntegral (BL.length message) then
                                               do id <- readOneByte
                                                  case id of
                                                       0 -> return $ Just ChokeMsg
                                                       1 -> return $ Just UnchokeMsg
                                                       2 -> return $ Just InterestedMsg
                                                       3 -> return $ Just NotInterestedMsg
                                                       4 -> do index <- readFourBytes
                                                               return $ Just $ HaveMsg index
                                                       5 -> do bitfield <- statefulSplit (len - 1)
                                                               return $ Just $ BitfieldMsg bitfield
                                                       6 -> do index  <- readFourBytes
                                                               begin  <- readFourBytes
                                                               length <- readFourBytes
                                                               return $ Just $ RequestMsg index begin length
                                                       7 -> do index <- readFourBytes
                                                               begin <- readFourBytes
                                                               block <- statefulSplit (len - 9)
                                                               return $ Just $ PieceMsg index begin (fromIntegral $ BL.length block) block
                                                       8 -> do index  <- readFourBytes
                                                               begin  <- readFourBytes
                                                               length <- readFourBytes
                                                               return $ Just $ CancelMsg index begin length
                                                       9 -> do port <- readTwoBytes
                                                               return $ Just $ PortMsg port
                                                       _ -> do put unparsed             --Alternatively can be statefulSplit (len-1)?
                                                               return Nothing
                                  else do put unparsed
                                          return Nothing
             else return Nothing

-- Use convert to parse a list of messages from the stream
parseMessages :: State BL.ByteString [Message]
parseMessages = do bs <- get
                   let (maybeMsg,bs') = runState convert bs
                   if isNothing maybeMsg
                   then return []
                   else do put bs'
                           xs <- parseMessages
                           return (fromJust maybeMsg:xs)

parseHandshake :: Stateless -> State BL.ByteString Bool
parseHandshake constants = do bs <- get
                              if BL.length bs >= 49 + fromIntegral (length pStr)
                              then do len <- readOneByte
                                      pStrByteString <- statefulSplit pStrLen
                                      reserved <- statefulSplit 8              -- Do we need to check reserved bytes?
                                      infoHash <- statefulSplit lenHash
                                      peerId <- statefulSplit lenHash
                                      if (len /= pStrLen || LC.unpack pStrByteString /= pStr || infoHash /= getHash (getInfoHash constants))
                                      then return False                        -- Should we take some action here?
                                      else return True
                              else return False

-- Convert the peer (stateful) to a new peer after appending the newly parsed messages using parseMessages
peerMessages :: State Peer ()
-- Can maybe also be written as Peer -> Peer
peerMessages = do peer@Handshake{ getUnparsed = bs, getPendingMessages = pMsgs } <- get
                  let (msgList,bs') = runState parseMessages bs
                  put peer{ getUnparsed = bs', getPendingMessages = pMsgs++msgList}
                  return ()

-- Convert a Message to ByteString to for sending
msgToByteStr :: Message -> BL.ByteString
msgToByteStr msg = toLazyByteString $ execWriter $ case msg of
                             KeepAliveMsg -> tell $ int32BE 0
                             ChokeMsg -> do tell $ int32BE 1
                                            tell $ int8 0
                             UnchokeMsg -> do tell $ int32BE 1
                                              tell $ int8 1
                             InterestedMsg -> do tell $ int32BE 1
                                                 tell $ int8 2
                             NotInterestedMsg -> do tell $ int32BE 1
                                                    tell $ int8 3
                             HaveMsg index -> do tell $ int32BE 5
                                                 tell $ int8 4
                                                 tell $ int32BE $ fromIntegral index
                             BitfieldMsg bitfield -> do tell $ int32BE $ fromIntegral $ 1 + BL.length bitfield
                                                        tell $ int8 5
                                                        tell $ lazyByteString bitfield
                             RequestMsg index begin length -> do tell $ int32BE 13
                                                                 tell $ int8 6
                                                                 tell $ int32BE $ fromIntegral index
                                                                 tell $ int32BE $ fromIntegral begin
                                                                 tell $ int32BE $ fromIntegral length
                             PieceMsg index begin blockLength block -> do tell $ int32BE $ fromIntegral $ 9 + blockLength
                                                                          tell $ int8 7
                                                                          tell $ int32BE $ fromIntegral index
                                                                          tell $ int32BE $ fromIntegral begin
                                                                          tell $ lazyByteString block
                             CancelMsg index begin length -> do tell $ int32BE 13
                                                                tell $ int8 8
                                                                tell $ int32BE $ fromIntegral index
                                                                tell $ int32BE $ fromIntegral begin
                                                                tell $ int32BE $ fromIntegral length
                             PortMsg port -> do tell $ int32BE 3
                                                tell $ int8 9
                                                tell $ int16BE $ fromIntegral port
