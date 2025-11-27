-- | Connection pool
module Bull.Pool
  ( Pool
  , withPool
  , connectNet
  , killNet
  , sendNet
  , recvNet
  , readNets
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

data Pool = Pool
  { lgr      :: Logger
  , ioChan   :: TChan (IO ())
  , ioMap    :: TVar (HashMap Net NetHandle)
  }

newPool :: Int -> Logger -> IO Pool
newPool _ l = Pool l <$> newTChanIO <*> newTVarIO mempty

withPool
  :: Int -- ^ max concurrent connections
  -> Logger
  -> (Pool -> IO a)
  -> IO a
withPool i l k = do
  p <- newPool i l
  runConcurrently $ asum $ map Concurrently $ k p : replicate i (worker p)

worker :: Pool -> IO a
worker = forever . join . atomically . readTChan . ioChan

data NetHandle = NetHandle
  { killVar  :: TMVar ()
  , sendChan :: TChan Msg
  , recvChan :: TChan Msg
  }

newNetHandle :: STM NetHandle
newNetHandle =
  NetHandle
    <$> newEmptyTMVar
    <*> newBroadcastTChan
    <*> newBroadcastTChan

connectNet :: Pool -> Net -> IO ()
connectNet p net = atomically $ do
  m <- readTVar $ ioMap p
  case M.lookup net m of
    Nothing -> do
      hndl <- newNetHandle
      modifyTVar' (ioMap p) $ M.insert net hndl
      s <- dupTChan $ sendChan hndl
      writeTChan (ioChan p) $ connection p net hndl s
    Just hndl -> do
      s <- dupTChan $ sendChan hndl
      writeTChan (ioChan p) $ connection p net hndl s

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

readNets :: Pool -> IO [Net]
readNets = atomically . fmap M.keys . readTVar . ioMap
