module Bull.Client
  ( Client
  , withClient
  , sendRpc
  , recvRpc
  ) where

import Bull.Log
import Bull.Server
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString.Lazy

data Client = Client
  { lgr      :: Logger
  , sendChan :: TChan Rpc
  , recvChan :: TChan Rpc
  }

newClient :: Logger -> IO Client
newClient l = Client l <$> newTChanIO <*> newBroadcastTChanIO

withClient
  :: String -- ^ host
  -> String -- ^ port
  -> Logger
  -> (Client -> IO a)
  -> IO a
withClient host port l k = runTCPClient host port $ \sock -> do
  client <- newClient l
  runConcurrently $ asum $ map Concurrently
    [ sender client sock
    , recver client sock
    , k client
    ]

sendRpc :: Client -> Rpc -> IO ()
sendRpc client = atomically . writeTChan (sendChan client)

recvRpc :: Client -> (IO Rpc -> IO a) -> IO a
recvRpc client k = do
  recvChan' <- atomically $ dupTChan $ recvChan client
  k $ atomically $ readTChan recvChan'

sender :: Client -> Socket -> IO a
sender client sock = forever $ do
  rpc <- atomically $ readTChan $ sendChan client
  sendAll sock $ encode rpc

recver :: Client -> Socket -> IO a
recver client sock = do
  say (lgr client) "decoding rpcs"
  runDecoder client mempty $ recv sock 4096

runDecoder :: Client -> ByteString -> IO ByteString -> IO a
runDecoder hndl bs bsIO = loop $ runGetIncremental get
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
