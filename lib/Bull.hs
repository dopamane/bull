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
  DaemonCli               -> daemon
  ClientCli host port rpc ->
    withLog $ \lgr ->
    withClient host port lgr $ \client ->
    recvRpc client $ \rpcIO -> do
      case rpc of
        Nets{} -> do
          sendRpc client rpc
          print . pretty =<< rpcIO
        _ -> do
          sendRpc client rpc
          forever $ print . pretty =<< rpcIO
