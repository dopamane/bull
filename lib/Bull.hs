module Bull
  ( bullMain
  , bullCli
  ) where

import Bull.Cli
import Bull.Client
import Bull.Daemon
import Bull.Log
import Bull.Server
import Control.Monad
import Prettyprinter

bullMain :: BullCli -> IO ()
bullMain cli = case cli of
  DaemonCli -> daemon
  ClientCli host port rpc ->
    withLog $ \lgr ->
    withClient host port lgr $ \client ->
    recvRpc client $ \rpcIO ->
      case rpc of
        Connect{} -> sendRpc client rpc
        Disconnect{} -> sendRpc client rpc
        Message{} -> fail "not implemented"
        Listen{} -> do
          sendRpc client rpc
          forever $ print . pretty =<< rpcIO
        Nets{} -> do
          sendRpc client rpc
          print . pretty =<< rpcIO
        Ping{} -> do
          say lgr "ping"
          sendRpc client rpc
          waitForPing rpcIO
          say lgr "pong"

waitForPing :: IO Rpc -> IO ()
waitForPing rpcIO = do
  rpc <- rpcIO
  case rpc of
    Ping{} -> return ()
    _      -> waitForPing rpcIO
