module Bull.Message
  ( BullMessageHandle
  , withBullMessage
  , sendBullMessage
  , recvBullMessage
  , withPingPong
  , BullMessage(..)
  , BullPayload(..)
  , toBullPayload
  , versionHandshake
  , sendGetAddrMsg
  , getMessage
  , putMessage
  , pongMsg
  , versionMsg
  , verackMsg
  , getAddrMsg
  ) where

import Bull.Client
import Bull.Log
import Bull.Message.Addr
import Bull.Message.Header
import Bull.Message.Version
import Bull.Net
import Bull.Pretty
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Prettyprinter

data BullMessageHandle = BullMessageHandle
  { lgr      :: LogHandle
  , client   :: BullClientHandle
  , net      :: BullNet
  , frClient :: TChan BullMessage
  }

newBullMessage
  :: LogHandle
  -> BullClientHandle
  -> BullNet
  -> IO BullMessageHandle
newBullMessage lgr' client' net' =
  BullMessageHandle lgr' client' net' <$> newBroadcastTChanIO

withBullMessage
  :: LogHandle
  -> BullClientHandle
  -> BullNet
  -> (BullMessageHandle -> IO a)
  -> IO a
withBullMessage lgr' client' net' k = do
  hndl <- newBullMessage lgr' client' net'
  either id id <$> race (runBullMessage hndl) (k hndl)

sendBullMessage :: BullMessageHandle -> BullMessage -> IO ()
sendBullMessage hndl = sendBullClient (client hndl) . runPut . putMessage

recvBullMessage
  :: BullMessageHandle
  -> (IO BullMessage -> IO a)
  -> IO a
recvBullMessage hndl k = do
  c <- atomically $ dupTChan $ frClient hndl
  k $ atomically $ readTChan c

runBullMessage :: BullMessageHandle -> IO a
runBullMessage hndl = recvBullClient (client hndl) $ \bsIO -> do
  say (lgr hndl) "running message decoder"
  runDecoder hndl mempty bsIO

runDecoder :: BullMessageHandle -> ByteString -> IO ByteString -> IO a
runDecoder hndl bs bsIO = loop $ runGetIncremental $ getMessage $ net hndl
  where
    loop decoder
      | L.null bs = pushChunks' =<< bsIO
      | otherwise = pushChunks' bs
      where
       pushChunks' bs' = do
        case pushChunks decoder bs' of
         Fail bs'' _ _      -> runDecoder hndl (L.fromStrict bs'') bsIO
         decoder'@Partial{} -> loop decoder'
         Done bs'' _ a      -> do
           atomically $ writeTChan (frClient hndl) a
           runDecoder hndl (L.fromStrict bs'') bsIO

data BullMessage = BullMessage
  { bmHeader  :: BullMessageHeader
  , bmPayload :: ByteString
  }
  deriving (Eq, Read, Show)

instance Pretty BullMessage where
  pretty msg = vsep
    [ pretty "message:"
    , indent 2 $ vsep
      [ pretty $ bmHeader msg
      , pretty "payload:"
      , indent 2 $ pretty $ L.length $ bmPayload msg
      ]
    ]

getMessage :: BullNet -> Get BullMessage
getMessage n = do
  hdr <- getHeader n
  let size = fromIntegral $ bmhPayloadSize hdr
  BullMessage hdr <$> getLazyByteString size

putMessage :: BullMessage -> Put
putMessage msg = do
  putHeader $ bmHeader msg
  putLazyByteString $ bmPayload msg

data BullPayload
  = BmpVersion BullVersionMsg
  | BmpVerack
  | BmpPing Word64
  | BmpPong Word64
  | BmpAddr AddrMsg
  | BmpGetAddr
  | BmpRaw ByteString
  deriving (Eq, Read, Show)

instance Pretty BullPayload where
  pretty p = case p of
    BmpVersion v -> pretty v
    BmpVerack    -> pretty "verack"
    BmpPing n    -> pretty "ping" <+> pretty n
    BmpPong n    -> pretty "pong" <+> pretty n
    BmpAddr a    -> pretty "addr" <+> pretty a
    BmpGetAddr   -> pretty "getaddr"
    BmpRaw bs    -> pretty "raw"  <+> prettyBytes bs

getBullPayload :: String -> Get BullPayload
getBullPayload commandName = case commandName of
  "version" -> BmpVersion <$> get <* eof
  "verack"  -> BmpVerack  <$  eof
  "ping"    -> BmpPing    <$> getWord64le <* eof
  "pong"    -> BmpPong    <$> getWord64le <* eof
  "addr"    -> BmpAddr    <$> get <* eof
  "getaddr" -> BmpGetAddr <$  eof
  _         -> BmpRaw     <$> getRemainingLazyByteString

eof :: Get ()
eof = guard =<< isEmpty

toBullPayload :: BullMessage -> BullPayload
toBullPayload m = runGet (getBullPayload cmdName) $ bmPayload m
  where
    cmdName = LC.unpack $ L.takeWhile (/= 0x00) $ bmhCommandName $ bmHeader m

-- | encode payload
putBullPayload :: BullPayload -> Put
putBullPayload p = case p of
  BmpVersion v -> put v
  BmpVerack    -> return ()
  BmpPing n    -> putWord64le n
  BmpPong n    -> putWord64le n
  BmpAddr a    -> put a
  BmpGetAddr   -> return ()
  BmpRaw bs    -> putLazyByteString bs

-- | construct a pong message from the nonce of a ping
pongMsg
  :: BullNet
  -> Word64 -- ^ nonce
  -> BullMessage
pongMsg n nonce = BullMessage
  { bmHeader  = mkBullMessageHeader (netStartString n) "pong" payload
  , bmPayload = payload
  }
  where
    payload = runPut $ putBullPayload $ BmpPong nonce

sendPongMsg
  :: BullMessageHandle
  -> Word64 -- ^ nonce from ping
  -> IO ()
sendPongMsg hndl = sendBullMessage hndl . pongMsg (net hndl)

-- | verack message constructor
verackMsg :: BullNet -> BullMessage
verackMsg = emptyMsg "verack"

sendVerackMsg :: BullMessageHandle -> IO ()
sendVerackMsg hndl = sendBullMessage hndl $ verackMsg $ net hndl

-- | respond to each ping with a pong
withPingPong :: BullMessageHandle -> IO a -> IO a
withPingPong hndl = fmap (either id id) . race loop
  where
    loop = recvBullMessage hndl $ \msgIO -> do
      say (lgr hndl) "running ping-pong"
      forever $ do
        payload <- toBullPayload <$> msgIO
        case payload of
          BmpPing nonce -> sendPongMsg hndl nonce
          _ -> return ()

versionHandshake :: BullMessageHandle -> IO ()
versionHandshake hndl = recvBullMessage hndl $ \msgIO -> do
  sendVersionMsg hndl
  recvVersionMsg msgIO
  recvVerackMsg msgIO
  sendVerackMsg hndl

recvVersionMsg :: IO BullMessage -> IO ()
recvVersionMsg msgIO = loop
  where
    loop = do
      payload <- toBullPayload <$> msgIO
      case payload of
        BmpVersion{} -> return ()
        _            -> loop

recvVerackMsg :: IO BullMessage -> IO ()
recvVerackMsg msgIO = loop
  where
    loop = do
      payload <- toBullPayload <$> msgIO
      case payload of
        BmpVerack -> return ()
        _         -> loop

sendVersionMsg :: BullMessageHandle -> IO ()
sendVersionMsg hndl = sendBullMessage hndl =<< versionMsg (net hndl)

versionMsg :: BullNet -> IO BullMessage
versionMsg n = do
  payload <- encode <$> mkVersionMsg n
  return BullMessage
    { bmHeader  = mkBullMessageHeader (netStartString n) "version" payload
    , bmPayload = payload
    }

getAddrMsg :: BullNet -> BullMessage
getAddrMsg = emptyMsg "getaddr"

sendGetAddrMsg :: BullMessageHandle -> IO ()
sendGetAddrMsg hndl = sendBullMessage hndl $ getAddrMsg (net hndl)

-- | message with no payload
emptyMsg :: String -> BullNet -> BullMessage
emptyMsg msg n = BullMessage
  { bmHeader   = mkBullMessageHeader (netStartString n) msg mempty
  , bmPayload  = mempty
  }
