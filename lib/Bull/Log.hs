module Bull.Log
  ( withLog
  , Logger
  , say
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

newtype Logger = Logger (TChan String)

withLog :: (Logger -> IO a) -> IO a
withLog k = do
  hndl <- Logger <$> newTChanIO
  either id id <$> race (runLog hndl) (k hndl)

runLog :: Logger -> IO a
runLog (Logger c) = forever $ putStrLn =<< atomically (readTChan c)

say :: Logger -> String -> IO ()
say (Logger c) = atomically . writeTChan c
