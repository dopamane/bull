module Bull
  ( bullMain
  , bullCli
  , BullCli(..)
  ) where

import Bull.Cli
import Bull.Client
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
  withBullClient lgr $ \client -> do
    connectBullClient client (netHost net) (netPort net)
    withBullMessage lgr client net $ \msgr ->
      logBullMessages lgr msgr $ do
        versionHandshake msgr
        withPingPong msgr $ do
          sendGetAddrMsg msgr
          forever $ threadDelay maxBound

logBullMessages :: LogHandle -> BullMessageHandle -> IO a -> IO a
logBullMessages lgr msgr = fmap (either id id) . race loop
  where
    loop = recvBullMessage msgr $ \msgIO ->
      forever $ do
        msg <- msgIO
        say lgr $ show $ vsep
          [ pretty msg
          , indent 4 $ pretty $ toBullPayload msg
          ]
