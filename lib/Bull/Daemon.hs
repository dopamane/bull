module Bull.Daemon
  ( daemon
  ) where

import Bull.Conn
import Bull.Log
import Bull.Message
import Bull.Pool
import Bull.Server
import Control.Concurrent.Async
import Control.Monad

daemon :: IO ()
daemon =
  withLog $ \lgr -> do
    say lgr "$$$ ₿itcoin ₿ull! $$$"
    withPool 1000 lgr $ \pool ->
      withServer "127.0.0.1" "8000" lgr $ \srvr ->
        recvServer srvr $ \rpcIO ->
          forever $ do
            rpc <- rpcIO
            case rpc of
              Connect net -> connect_ pool net $ \conn ->
                recvMsg conn $ \msgIO ->
                  concurrently_ (passMsgs srvr msgIO) $
                    sendMsg conn $ getAddrMsg net
              Disconnect net -> disconnect pool net
              Message{} -> return ()
              Nets{} -> sendServer srvr . Nets =<< readNets pool

passMsgs :: Server -> IO Msg -> IO a
passMsgs srvr msgIO = forever $ sendServer srvr . Message =<< msgIO
