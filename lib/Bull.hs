module Bull
  ( bullMain
  , bullCli
  , BullCli(..)
  ) where

import Bull.Cli
import Bull.Client
import Bull.Message
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Prettyprinter

bullMain :: BullCli -> IO ()
bullMain cli = case cli of
  BullClientCli net -> runClient net

runClient :: BullNet -> IO ()
runClient net = withBullClient $ \client -> do
  connectBullClient client (bullHost net) (bullPort net)
  withBullMessage client net $ \msgr ->
    logBullMessages msgr $ do
        versionHandshake msgr
        withPingPong msgr $ forever $ threadDelay maxBound

logBullMessages :: BullMessageHandle -> IO a -> IO a
logBullMessages hndl = fmap (either id id) . race loop
  where
    loop = recvBullMessage hndl $ \msgIO ->
      forever $ do
        msg <- msgIO
        print $ vsep
          [ pretty msg
          , indent 4 $ pretty $ toBullPayload msg
          ]
