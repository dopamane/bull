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
  , ioMap    :: TVar (HashMap BullNet (TMVar ()))
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

connect :: Pool -> BullNet -> (Conn -> IO a) -> IO (IO a)
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

connect_ :: Pool -> BullNet -> (Conn -> IO a) -> IO ()
connect_ p n = void . connect p n

connection :: Pool -> BullNet -> (Conn -> IO a) -> TMVar () -> IO a
connection p n k d =
  either id id <$> race (killer d) (withConn n (lgr p) k)
    `finally` deleteConnection p n

killer :: TMVar () -> IO a
killer d = atomically $ do
  check =<< isEmptyTMVar d
  throwSTM $ userError "connection killed"

deleteConnection :: Pool -> BullNet -> IO ()
deleteConnection p n = atomically $ modifyTVar' (ioMap p) $ M.delete n

disconnect :: Pool -> BullNet -> IO ()
disconnect p n = atomically $ do
  m <- readTVar $ ioMap p
  mapM_ takeTMVar $ M.lookup n m

connected :: Pool -> BullNet -> STM Bool
connected p n = M.member n <$> readTVar (ioMap p)
