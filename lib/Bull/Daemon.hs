module Bull.Daemon
  ( daemon
  ) where

import Bull.Conn
import Bull.Log
import Bull.Message
import Bull.Net
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Prettyprinter

daemon :: BullNet -> IO ()
daemon net =
  withLog $ \lgr -> do
    say lgr "$$$ ₿itcoin ₿ull! $$$"
    withConn net lgr $ \conn ->
      logMessages lgr conn $ do
        sendMsg conn $ getAddrMsg net
        forever $ threadDelay maxBound

logMessages :: Logger -> Conn -> IO a -> IO a
logMessages lgr conn = fmap (either id id) . race loop
  where
    loop = recvMsg conn $ \msgIO ->
      forever $ do
        msg <- msgIO
        say lgr $ show $ vsep
          [ pretty msg
          , indent 4 $ pretty $ toBullPayload msg
          ]
