module Bull.Daemon
  ( daemon
  ) where

import Bull.Log
import Bull.Message.Addr
import Bull.Pool
import Bull.Server
import Control.Monad

daemon :: IO ()
daemon =
  withLog $ \lgr -> do
    say lgr "$$$ ₿itcoin ₿ull! $$$"
    withAddrSet 1000 $ \addrSet ->
      withPool 1000 lgr addrSet $ \pool ->
      withServer "127.0.0.1" "8000" lgr $ \srvr ->
      recvServer srvr $ \rpcIO ->
      forever $ do
        rpc <- rpcIO
        case rpc of
          Connect net -> connectNet pool net
          Disconnect net -> killNet pool net
          Message{} -> fail "not implemented"
          Listen net -> recvNet pool net $ \msgIO ->
            forever $ sendServer srvr . Message =<< msgIO
          Nets{} -> sendServer srvr . Nets =<< readNets pool
          Ping net -> do
            pingNet pool net
            sendServer srvr $ Ping net
          ReadAddrs{} -> sendServer srvr . ReadAddrs =<< readAddrSet addrSet
