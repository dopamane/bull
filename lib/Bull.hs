module Bull
  ( bullMain
  , bullCli
  , BullCli(..)
  ) where

import Bull.Cli
import Bull.Client
import Bull.Message
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Binary
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Foreign.C
import Prettyprinter
import System.Posix

bullMain :: BullCli -> IO ()
bullMain cli = case cli of
  BullClientCli host port -> runClient host port

runClient :: String -> String -> IO ()
runClient host port = withBullClient $ \client -> do
  connectBullClient client host port
  withBullMessage client BullMainnet $ \msgr ->
    logBullMessages msgr $
      withBullPingPong msgr $ do
        print . pretty =<< bvm
        putStrLn "sending version message"
        recvBullMessage msgr $ \msgIO -> do
          sendBullMessage msgr =<< verMsg
          _ <- msgIO
          _ <- msgIO
          sendBullMessage msgr $ mkVerackMsg mainnetStartString
        forever $ threadDelay maxBound

verMsg :: IO BullMessage
verMsg = do
  payload <- encode <$> bvm
  return BullMessage
    { bmHeader  = mkBullMessageHeader mainnetStartString "version" payload
    , bmPayload = payload
    }

bvm :: IO BullVersionMsg
bvm = do
  CTime ts <- epochTime
  return BullVersionMsg
    { bvmVersion        = 70015
    , bvmServices       = 0x00
    , bvmTimestamp      = ts
    , bvmAddrRxSvc      = 0x00
    , bvmAddrRxIp       = L.replicate 10 0x00 <> L.pack [0xff, 0xff, 206, 206, 109, 24]
    , bvmAddrRxPort     = 8333
    , bvmAddrTxSvc      = 0x00
    , bvmAddrTxIp       = loopback
    , bvmAddrTxPort     = 8333
    , bvmNonce          = 0
    , bvmUserAgentBytes = 0
    , bvmUserAgent      = mempty
    , bvmStartHeight    = 0
    , bvmRelay          = Nothing
    }

-- | ipv6 @::ffff:127.0.0.1@
loopback :: ByteString
loopback = L.replicate 10 0x00 <> L.pack [0xff, 0xff, 0x7f, 0x00, 0x00, 0x01]

logBullMessages :: BullMessageHandle -> IO a -> IO a
logBullMessages hndl = fmap (either id id) . race loop
  where
    loop = recvBullMessage hndl $ \msgIO ->
      forever $ do
        msg <- msgIO
        print $ vsep
          [ pretty msg
          , indent 4 $ pretty $ toBullPayload msg
          ]
