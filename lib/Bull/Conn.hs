-- | Network connection
module Bull.Conn
  ( ConnHandle
  , withConn
  , sendMsg
  , recvMsg
  ) where

import Bull.Log
import Bull.Message
import Bull.Net
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString.Lazy
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L

data ConnHandle = ConnHandle
  { lgr      :: LogHandle
  , net      :: BullNet
  , sendChan :: TChan BullMessage
  , recvChan :: TChan BullMessage
  }

newConn :: LogHandle -> BullNet -> IO ConnHandle
newConn l n =
  ConnHandle l n
    <$> newTChanIO
    <*> newBroadcastTChanIO

withConn :: LogHandle -> BullNet -> (ConnHandle -> IO a) -> IO a
withConn l n k = runTCPClient (netHost n) (netPort n) $ \sock -> do
  hndl <- newConn l n
  runConcurrently $ asum $ map Concurrently
    [ sender hndl sock
    , recver hndl sock
    , k hndl
    ]

sendMsg :: ConnHandle -> BullMessage -> IO ()
sendMsg hndl = atomically . writeTChan (sendChan hndl)

recvMsg :: ConnHandle -> (IO BullMessage -> IO a) -> IO a
recvMsg hndl k = do
  recvChan' <- atomically $ dupTChan $ recvChan hndl
  k $ atomically $ readTChan recvChan'

sender :: ConnHandle -> Socket -> IO a
sender hndl sock = forever $ do
  msg <- atomically $ readTChan $ sendChan hndl
  sendAll sock $ runPut $ putMessage msg

recver :: ConnHandle -> Socket -> IO a
recver hndl sock = do
  say (lgr hndl) "decoding"
  runDecoder hndl mempty $ recv sock 4096

runDecoder :: ConnHandle -> ByteString -> IO ByteString -> IO a
runDecoder hndl bs bsIO = loop $ runGetIncremental $ getMessage $ net hndl
  where
    loop decoder
      | L.null bs = pushChunks' =<< bsIO
      | otherwise = pushChunks' bs
      where
       pushChunks' bs' = do
        case pushChunks decoder bs' of
         Fail bs'' _ _      -> runDecoder hndl (L.fromStrict bs'') bsIO
         decoder'@Partial{} -> loop decoder'
         Done bs'' _ a      -> do
           atomically $ writeTChan (recvChan hndl) a
           runDecoder hndl (L.fromStrict bs'') bsIO
