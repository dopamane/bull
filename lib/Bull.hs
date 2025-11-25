module Bull
  ( bullMain
  , bullCli
  ) where

import Bull.Cli
import Bull.Client
import Bull.Daemon
import Bull.Log

bullMain :: BullCli -> IO ()
bullMain cli = case cli of
  DaemonCli               -> daemon
  ClientCli host port rpc ->
    withLog $ \lgr ->
    withClient host port lgr $ \client ->
      sendRpc client rpc
