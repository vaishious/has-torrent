module Communicate where
import Types
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Maybe (fromJust,isNothing)
import Data.Word
import qualified Data.Binary as BY

readOneByte :: State BL.ByteString Int
readOneByte = state $ \bs -> let (left,right) = BL.splitAt 1 bs
                                 word8 = BY.decode left :: Word8
                             in (fromIntegral word8,right)

readFourBytes :: State BL.ByteString Int
readFourBytes = state $ \bs -> let (left,right) = BL.splitAt 4 bs
                                   word32 = BY.decode left :: Word32
                               in (fromIntegral word32,right)

statefulSplit :: Int -> State BL.ByteString BL.ByteString
statefulSplit index = state $ \bs -> BL.splitAt (fromIntegral index) bs

convert :: State BL.ByteString (Maybe Message)
convert = do message <- get
             if BL.length message > 3
             then do len <- readFourBytes
                     case len of
                          0 -> return $ Just KeepAliveMsg
                          _ -> do message <- get
                                  if len >= fromIntegral (BL.length message) then
                                               do id <- readOneByte
                                                  case id of
                                                       0 -> return $ Just ChokeMsg
                                                       1 -> return $ Just UnchokeMsg
                                                       2 -> return $ Just InterestedMsg
                                                       3 -> return $ Just NotInterestedMsg
                                                       4 -> do index <- readFourBytes
                                                               return $ Just $ HaveMsg index
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
                                                       _ -> return Nothing
                                  else return Nothing
             else return Nothing

parseMessages :: State BL.ByteString [Message]
parseMessages = do bs <- get
                   let (maybeMsg,bs') = runState convert bs
                   if isNothing maybeMsg
                   then return []
                   else do put bs'
                           xs <- parseMessages
                           return (fromJust maybeMsg:xs)

msgToByteStr :: Message -> BL.ByteString
msgToByteStr msg =  toLazyByteString $ execWriter $ case msg of
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
