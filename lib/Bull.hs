module Bull
  ( bullMain
  , bullCli
  ) where

import Bull.Cli
import Bull.Daemon

bullMain :: BullCli -> IO ()
bullMain cli = case cli of
  DaemonCli net -> daemon net

