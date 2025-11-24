-- | Network client API
module Bull.Client
  ( BullClientHandle
  , withBullClient
  , connectBullClient
  , sendBullClient
  , recvBullClient
  ) where

import Bull.Log
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString.Lazy

data BullClientHandle = BullClientHandle
  { lgr    :: LogHandle
  , host   :: TMVar String
  , port   :: TMVar String
  , toSock :: TChan ByteString
  , frSock :: TChan ByteString
  }

newBullClient :: LogHandle -> IO BullClientHandle
newBullClient l =
  BullClientHandle l
    <$> newEmptyTMVarIO
    <*> newEmptyTMVarIO
    <*> newTChanIO
    <*> newBroadcastTChanIO

connectBullClient
  :: BullClientHandle
  -> String -- ^ host
  -> String -- ^ port
  -> IO ()
connectBullClient hndl host' port' = atomically $ do
  writeTMVar (host hndl) host'
  writeTMVar (port hndl) port'

sendBullClient :: BullClientHandle -> ByteString -> IO ()
sendBullClient hndl = atomically . writeTChan (toSock hndl)

recvBullClient :: BullClientHandle -> (IO ByteString -> IO a) -> IO a
recvBullClient hndl k = do
  c <- atomically $ dupTChan $ frSock hndl
  k $ atomically $ readTChan c

withBullClient :: LogHandle -> (BullClientHandle -> IO a) -> IO a
withBullClient l k = do
  hndl <- newBullClient l
  either id id <$> race (runBullClient hndl) (k hndl)

runBullClient :: BullClientHandle -> IO a
runBullClient hndl = join $ atomically $ do
  host' <- readTMVar $ host hndl
  port' <- readTMVar $ port hndl
  return $ runTCPClient host' port' $ tcpClient hndl

tcpClient :: BullClientHandle -> Socket -> IO a
tcpClient hndl sock = do
  say (lgr hndl) "running tcp client"
  either id id <$> race (sending hndl sock) (recving hndl sock)

sending :: BullClientHandle -> Socket -> IO a
sending hndl sock = forever $ do
  bs <- atomically $ readTChan $ toSock hndl
  sendAll sock bs

recving :: BullClientHandle -> Socket -> IO a
recving hndl sock = forever $ do
  bs <- recv sock 4096
  when (L.null bs) $ fail "recv closed"
  atomically $ writeTChan (frSock hndl) bs
