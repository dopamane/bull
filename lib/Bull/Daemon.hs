module Bull.Daemon
  ( daemon
  ) where

import Bull.Conn
import Bull.Log
import Bull.Message
import Bull.Pool
import Bull.Server
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Prettyprinter

daemon :: IO ()
daemon =
  withLog $ \lgr -> do
    say lgr "$$$ ₿itcoin ₿ull! $$$"
    withPool 1 lgr $ \pool ->
      withServer "127.0.0.1" "8000" lgr $ \srvr ->
        recvServer srvr $ \rpcIO ->
          forever $ do
            rpc <- rpcIO
            case rpc of
              Connect net -> connect_ pool net $ \conn ->
                logMessages lgr conn $ forever $ threadDelay maxBound
              Disconnect net -> disconnect pool net

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
