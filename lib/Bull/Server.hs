{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Bull.Server
  ( Server
  , withServer
  , sendServer
  , recvServer
  , Rpc(..)
  ) where

import Bull.Log
import Bull.Net
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import GHC.Generics
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString.Lazy

data Server = Server
  { lgr :: Logger
  , sendChan :: TChan Rpc
  , recvChan :: TChan Rpc
  }

newServer :: Logger -> IO Server
newServer l = Server l <$> newTChanIO <*> newBroadcastTChanIO

withServer
  :: String -- ^ host
  -> String -- ^ port
  -> Logger
  -> (Server -> IO a)
  -> IO a
withServer host port l k = do
  srvr <- newServer l
  runTCPServer (Just host) port $ \sock ->
    runConcurrently $ asum $ map Concurrently
      [ sender srvr sock
      , recver srvr sock
      , k srvr
      ]

sendServer :: Server -> Rpc -> IO ()
sendServer srvr = atomically . writeTChan (sendChan srvr)

recvServer :: Server -> (IO Rpc -> IO a) -> IO a
recvServer srvr k = do
  recvChan' <- atomically $ dupTChan $ recvChan srvr
  k $ atomically $ readTChan recvChan'

sender :: Server -> Socket -> IO a
sender srvr sock = forever $ do
  rpc <- atomically $ readTChan $ sendChan srvr
  sendAll sock $ encode rpc

recver :: Server -> Socket -> IO a
recver srvr sock = do
  say (lgr srvr) "decoding rpcs"
  runDecoder srvr mempty $ recv sock 4096

runDecoder :: Server -> ByteString -> IO ByteString -> IO a
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

data Rpc
  = Connect    Net
  | Disconnect Net
  deriving (Binary, Eq, Generic, Read, Show)
