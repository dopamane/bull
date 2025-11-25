module Bull.Daemon
  ( daemon
  ) where

import Bull.Conn
import Bull.Log
import Bull.Message
import Bull.Net
import Bull.Pool
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Prettyprinter

daemon :: Net -> IO ()
daemon net =
  withLog $ \lgr -> do
    say lgr "$$$ ₿itcoin ₿ull! $$$"
    withPool 1 lgr $ \pool -> do
      r <- connect pool net $ \conn ->
        logMessages lgr conn $ do
          sendMsg conn $ getAddrMsg net
          threadDelay 10000000
      threadDelay 5000000
      disconnect pool net
      say lgr . show =<< r

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
