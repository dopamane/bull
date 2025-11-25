-- | Connection pool
module Bull.Pool
  ( Pool
  , withPool
  , connect
  , connect_
  , disconnect
  , connected
  ) where

import Bull.Conn
import Bull.Log
import Bull.Net
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

data Pool = Pool
  { maxConns :: Int
  , lgr      :: Logger
  , ioChan   :: TChan (IO ())
  , ioMap    :: TVar (HashMap Net (TMVar ()))
  }

newPool :: Int -> Logger -> IO Pool
newPool i l = Pool i l <$> newTChanIO <*> newTVarIO mempty

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

connect :: Pool -> Net -> (Conn -> IO a) -> IO (IO (Maybe a))
connect p net k = atomically $ do
  m <- readTVar $ ioMap p
  when (M.size m >= maxConns p) $ throwSTM $ userError "exceeds max connections"
  when (M.member net m)         $ throwSTM $ userError "already connected"
  r <- newEmptyTMVar
  d <- newTMVar ()
  writeTChan (ioChan p) $ atomically . putTMVar r =<< try (connection p net k d)
  modifyTVar' (ioMap p) $ M.insert net d
  return $ result r
  where
    result r = do
      r' <- atomically $ takeTMVar r
      case r' of
        Left  e -> throwIO (e :: SomeException)
        Right a -> return a

connect_ :: Pool -> Net -> (Conn -> IO a) -> IO ()
connect_ p n = void . connect p n

connection :: Pool -> Net -> (Conn -> IO a) -> TMVar () -> IO (Maybe a)
connection p n k d =
  either (const $ Nothing) Just <$> race (killer d) (withConn n (lgr p) k)
    `finally` deleteConnection p n

killer :: TMVar () -> IO ()
killer d = atomically $ check =<< isEmptyTMVar d

deleteConnection :: Pool -> Net -> IO ()
deleteConnection p n = atomically $ modifyTVar' (ioMap p) $ M.delete n

disconnect :: Pool -> Net -> IO ()
disconnect p n = atomically $ do
  m <- readTVar $ ioMap p
  mapM_ takeTMVar $ M.lookup n m

connected :: Pool -> Net -> STM Bool
connected p n = M.member n <$> readTVar (ioMap p)
