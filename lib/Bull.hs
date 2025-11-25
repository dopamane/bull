module Bull
  ( bullMain
  , bullCli
  , BullCli(..)
  ) where

import Bull.Cli
import Bull.Conn
import Bull.Log
import Bull.Message
import Bull.Net
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Prettyprinter

bullMain :: BullCli -> IO ()
bullMain cli = case cli of
  BullClientCli net -> runClient net

runClient :: BullNet -> IO ()
runClient net =
  withLog $ \lgr ->
  withConn net lgr $ \conn ->
  logMessages lgr conn $ do
    sendMsg conn $ getAddrMsg net
    forever $ threadDelay maxBound

logMessages :: LogHandle -> ConnHandle -> IO a -> IO a
logMessages lgr conn = fmap (either id id) . race loop
  where
    loop = recvMsg conn $ \msgIO ->
      forever $ do
        msg <- msgIO
        say lgr $ show $ vsep
          [ pretty msg
          , indent 4 $ pretty $ toBullPayload msg
          ]
