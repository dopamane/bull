module Bull.Log
  ( withLog
  , LogHandle
  , say
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

newtype LogHandle = LogHandle (TChan String)

withLog :: (LogHandle -> IO a) -> IO a
withLog k = do
  hndl <- LogHandle <$> newTChanIO
  either id id <$> race (runLog hndl) (k hndl)

runLog :: LogHandle -> IO a
runLog (LogHandle c) = forever $ putStrLn =<< atomically (readTChan c)

say :: LogHandle -> String -> IO ()
say (LogHandle c) = atomically . writeTChan c
