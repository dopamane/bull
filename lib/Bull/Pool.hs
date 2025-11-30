-- | Connection pool
module Bull.Pool
  ( Pool
  , withPool
  , connectNet
  , killNet
  , sendNet
  , recvNet
  , readNets
  , pingNet
  ) where

import Bull.Conn
import Bull.Log
import Bull.Message
import Bull.Net
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import System.Random.MWC

data Pool = Pool
  { lgr    :: Logger
  , ioChan :: TChan (IO ())
  , ioMap  :: TVar (HashMap Net NetHandle)
  , rand   :: GenIO
  }

newPool :: Logger -> IO Pool
newPool l =
  Pool l
    <$> newTChanIO
    <*> newTVarIO mempty
    <*> createSystemRandom

withPool
  :: Int -- ^ max concurrent connections
  -> Logger
  -> (Pool -> IO a)
  -> IO a
withPool i l k = do
  p <- newPool l
  runConcurrently $ asum $ map Concurrently $ k p : replicate i (worker p)

worker :: Pool -> IO a
worker = forever . join . atomically . readTChan . ioChan

data NetHandle = NetHandle
  { killVar  :: TMVar ()
  , sendChan :: TChan Msg
  , recvChan :: TChan Msg
  , connVar  :: TVar NetStatus
  }

newNetHandle :: STM NetHandle
newNetHandle =
  NetHandle
    <$> newEmptyTMVar
    <*> newBroadcastTChan
    <*> newBroadcastTChan
    <*> newTVar Offline

connectNet :: Pool -> Net -> IO ()
connectNet p net = atomically $ do
  m <- readTVar $ ioMap p
  hndl <- case M.lookup net m of
    Nothing -> do
      hndl <- newNetHandle
      modifyTVar' (ioMap p) $ M.insert net hndl
      return hndl
    Just hndl -> return hndl
  status <- readTVar $ connVar hndl
  when (status == Offline) $ do
    writeTVar (connVar hndl) Online
    s <- dupTChan $ sendChan hndl
    writeTChan (ioChan p) $
      connection p net hndl s `finally` disconnected hndl

disconnected :: NetHandle -> IO ()
disconnected hndl = atomically $ writeTVar (connVar hndl) Offline

handleException :: IO () -> IO ()
handleException = flip catches
  [ Handler (throwIO           :: SomeAsyncException -> IO ())
  , Handler (const $ return () :: SomeException      -> IO ())
  ]

connection :: Pool -> Net -> NetHandle -> TChan Msg -> IO ()
connection p n hndl s =
  handleException $
  race_ (killer hndl) $
  withConn n (lgr p) $ \conn ->
  race_ (sender conn s) (recver hndl conn)

sender :: Conn -> TChan Msg -> IO a
sender conn s = forever $ sendMsg conn =<< atomically (readTChan s)

recver :: NetHandle -> Conn -> IO a
recver hndl conn = recvMsg conn $ \msgIO ->
  forever $ atomically . writeTChan (recvChan hndl) =<< msgIO

killer :: NetHandle -> IO ()
killer = atomically . takeTMVar . killVar

sendNet :: Pool -> Net -> Msg -> IO ()
sendNet p n msg = atomically $ do
  hndl <- (M.! n) <$> readTVar (ioMap p)
  writeTChan (sendChan hndl) msg

recvNet :: Pool -> Net -> (IO Msg -> IO a) -> IO a
recvNet p n k = do
  r <- atomically $ do
    hndl <- (M.! n) <$> readTVar (ioMap p)
    dupTChan $ recvChan hndl
  k $ atomically $ readTChan r

killNet :: Pool -> Net -> IO ()
killNet p n = atomically $ do
  hndlM <- M.lookup n <$> readTVar (ioMap p)
  case hndlM of
    Nothing   -> return ()
    Just hndl -> writeTMVar (killVar hndl) ()

readNets :: Pool -> IO [(Net, NetStatus)]
readNets p = atomically $ do
  m <- readTVar $ ioMap p
  forM (M.toList m) $ \(n, hndl) ->
    (,) n <$> readTVar (connVar hndl)

pingNet :: Pool -> Net -> IO ()
pingNet p n = recvNet p n $ \msgIO -> do
  nonce <- uniformM $ rand p
  sendNet p n $ pingMsg n nonce
  waitForPong msgIO nonce
  where
    waitForPong msgIO nonce = do
      pongPayload <- toBullPayload <$> msgIO
      case pongPayload of
        BmpPong nonce'
          | nonce' == nonce -> return ()
        _                   -> waitForPong msgIO nonce
