module Bull.Message
  ( BullMessageHandle
  , withBullMessage
  , sendBullMessage
  , recvBullMessage
  , BullMessage(..)
  , BullMessageHeader(..)
  , mkBullMessageHeader
  , BullPayload(..)
  , toBullPayload
  , BullVersionMsg(..)
  , mkPongMsg
  , mkVerackMsg
  , sendVerackMsg
  , versionHandshake
  , BullNet(..)
  , bullMainnet
  , bullTestnet
  ) where

import Bull.Client
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Digest.Pure.SHA
import Data.Function
import Data.IP
import Data.Int
import Foreign.C
import Numeric
import Prettyprinter
import System.Posix

data BullMessageHandle = BullMessageHandle
  { client   :: BullClientHandle
  , net      :: BullNet
  , frClient :: TChan BullMessage
  }

newBullMessage
  :: BullClientHandle
  -> BullNet
  -> IO BullMessageHandle
newBullMessage client' net' =
  BullMessageHandle client' net' <$> newBroadcastTChanIO

withBullMessage
  :: BullClientHandle
  -> BullNet
  -> (BullMessageHandle -> IO a)
  -> IO a
withBullMessage client' net' k = do
  hndl <- newBullMessage client' net'
  either id id <$> race (runBullMessage hndl) (k hndl)

sendBullMessage :: BullMessageHandle -> BullMessage -> IO ()
sendBullMessage hndl = sendBullClient (client hndl) . encode

recvBullMessage
  :: BullMessageHandle
  -> (IO BullMessage -> IO a)
  -> IO a
recvBullMessage hndl k = do
  c <- atomically $ dupTChan $ frClient hndl
  k $ atomically $ readTChan c

runBullMessage :: BullMessageHandle -> IO a
runBullMessage hndl =
  withPingPong hndl $
  recvBullClient (client hndl) $
    runDecoder hndl mempty

runDecoder :: BullMessageHandle -> ByteString -> IO ByteString -> IO a
runDecoder hndl bs bsIO = loop $ runGetIncremental get
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

instance Binary BullMessage where
  get = getMessage
  put = putMessage

getMessage :: Get BullMessage
getMessage = do
  hdr <- get
  let size = fromIntegral $ bmhPayloadSize hdr
  BullMessage hdr <$> getLazyByteString size

putMessage :: BullMessage -> Put
putMessage msg = do
  put $ bmHeader msg
  putLazyByteString $ bmPayload msg

data BullMessageHeader = BullMessageHeader
  { bmhStartString :: ByteString
  , bmhCommandName :: ByteString
  , bmhPayloadSize :: Word32
  , bmhChecksum    :: ByteString
  }
  deriving (Eq, Read, Show)

instance Pretty BullMessageHeader where
  pretty hdr = vsep
    [ pretty "header:"
    , indent 2 $ vsep
      [ pretty "start string:" <+> prettyBytes (bmhStartString hdr)
      , pretty "command name:" <+> renderCommandName hdr
      , pretty "payload size:" <+> pretty (bmhPayloadSize hdr)
      , pretty "checksum:    " <+> prettyBytes (bmhChecksum hdr)
      ]
    ]

-- | Message header smart-constructor
mkBullMessageHeader
  :: ByteString -- ^ start string 'mainnetStartString'
  -> String     -- ^ command name
  -> ByteString -- ^ payload
  -> BullMessageHeader
mkBullMessageHeader startString commandName payload = BullMessageHeader
  { bmhStartString = startString
  , bmhCommandName = L.take 12 $ LC.pack commandName <> L.replicate 12 0x00
  , bmhPayloadSize = fromIntegral $ L.length payload
  , bmhChecksum    = checksum
  }
  where
    checksum = sha256 payload
                 & bytestringDigest
                 & sha256
                 & bytestringDigest
                 & L.take 4

prettyBytes :: ByteString -> Doc ann
prettyBytes = foldMap renderByte . L.unpack
  where
    renderByte byt = renderNyb (byt `shiftR` 4) <> renderNyb (byt .&. 0xf)
    renderNyb  nyb = pretty $ showHex nyb ""

renderCommandName :: BullMessageHeader -> Doc ann
renderCommandName =
  pretty . LC.unpack . L.takeWhile (/= 0x00) . bmhCommandName

instance Binary BullMessageHeader where
  get = getHeader
  put = putHeader

getHeader :: Get BullMessageHeader
getHeader =
  BullMessageHeader
    <$> getStartString
    <*> getLazyByteString 12
    <*> getWord32le
    <*> getLazyByteString 4

-- | validate start string
getStartString :: Get ByteString
getStartString = do
  s <- getLazyByteString 4
  when (s /= mainnetStartString && s /= testnetStartString) $
    fail "invalid start string"
  return s

mainnetStartString :: ByteString
mainnetStartString = L.pack [0xF9, 0xBE, 0xB4, 0xD9]

testnetStartString :: ByteString
testnetStartString = L.pack [0x0B, 0x11, 0x09, 0x07]

putHeader :: BullMessageHeader -> Put
putHeader hdr = do
  putLazyByteString $ bmhStartString hdr
  putLazyByteString $ bmhCommandName hdr
  putWord32le       $ bmhPayloadSize hdr
  putLazyByteString $ bmhChecksum    hdr

data BullNet = BullNet
  { bullHost        :: String
  , bullPort        :: String
  , bullStartString :: ByteString
  }
  deriving (Eq, Read, Show)

bullMainnet :: String -> BullNet
bullMainnet host = BullNet
  { bullHost        = host
  , bullPort        = "8333"
  , bullStartString = mainnetStartString
  }

bullTestnet :: String -> BullNet
bullTestnet host = BullNet
  { bullHost = host
  , bullPort = "18333"
  , bullStartString = testnetStartString
  }

data BullVersionMsg = BullVersionMsg
  { bvmVersion        :: Int32
  , bvmServices       :: Word64
  , bvmTimestamp      :: Int64
  , bvmAddrRxSvc      :: Word64
  , bvmAddrRxIp       :: ByteString
  , bvmAddrRxPort     :: Word16
  , bvmAddrTxSvc      :: Word64
  , bvmAddrTxIp       :: ByteString
  , bvmAddrTxPort     :: Word16
  , bvmNonce          :: Word64
  , bvmUserAgentBytes :: Word8
  , bvmUserAgent      :: ByteString
  , bvmStartHeight    :: Int32
  , bvmRelay          :: Maybe Bool
  }
  deriving (Eq, Read, Show)

instance Pretty BullVersionMsg where
  pretty m = vsep
    [ pretty "version:"
    , indent 2 $ vsep
      [ ppretty putInt32le  (bvmVersion m)   <+> pretty "Protocol"     <+> pretty (bvmVersion m)
      , ppretty putWord64le (bvmServices m)  <+> pretty "Services"     <+> pretty (bvmServices m)
      , ppretty putInt64le  (bvmTimestamp m) <+> pretty "Epoch time"   <+> pretty (bvmTimestamp m)
      , ppretty putWord64le (bvmAddrRxSvc m) <+> pretty "Rx services"  <+> pretty (bvmAddrRxSvc m)
      , ppretty putLazyByteString (bvmAddrRxIp m) <+> pretty "Rx IP"
      , ppretty putWord16be (bvmAddrRxPort m) <+> pretty "Rx port" <+> pretty (bvmAddrRxPort m)
      , ppretty putWord64le (bvmAddrTxSvc m) <+> pretty "Tx services" <+> pretty (bvmAddrTxSvc m)
      , ppretty putLazyByteString (bvmAddrTxIp m) <+> pretty "Tx IP"
      , ppretty putWord16be (bvmAddrTxPort m) <+> pretty "Tx port" <+> pretty (bvmAddrTxPort m)
      ]
    ]

ppretty :: (a -> Put) -> a -> Doc ann
ppretty put' a = prettyBytes bs <+> pretty pad
  where
    bs  = runPut $ put' a
    len = fromIntegral $ L.length bs
    pad = replicate (35 - 2 * len) '.'

instance Binary BullVersionMsg where
  get = getBullVersionMsg
  put = putBullVersionMsg

getBullVersionMsg :: Get BullVersionMsg
getBullVersionMsg = do
  versn          <- getInt32le
  services       <- getWord64le
  timestamp      <- getInt64le
  rxSvc          <- getWord64le
  rxIp           <- getLazyByteString 16
  rxPort         <- getWord16be
  txSvc          <- getWord64le
  txIp           <- getLazyByteString 16
  txPort         <- getWord16be
  nonce          <- getWord64le
  userAgentBytes <- getWord8
  userAgent      <- getLazyByteString $ fromIntegral userAgentBytes
  startHeight    <- getInt32le
  relayM         <- optional get
  return BullVersionMsg
    { bvmVersion        = versn
    , bvmServices       = services
    , bvmTimestamp      = timestamp
    , bvmAddrRxSvc      = rxSvc
    , bvmAddrRxIp       = rxIp
    , bvmAddrRxPort     = rxPort
    , bvmAddrTxSvc      = txSvc
    , bvmAddrTxIp       = txIp
    , bvmAddrTxPort     = txPort
    , bvmNonce          = nonce
    , bvmUserAgentBytes = userAgentBytes
    , bvmUserAgent      = userAgent
    , bvmStartHeight    = startHeight
    , bvmRelay          = relayM
    }

putBullVersionMsg :: BullVersionMsg -> Put
putBullVersionMsg m = do
  putInt32le        $ bvmVersion     m
  putWord64le       $ bvmServices    m
  putInt64le        $ bvmTimestamp   m
  putWord64le       $ bvmAddrRxSvc   m
  putLazyByteString $ bvmAddrRxIp    m
  putWord16be       $ bvmAddrRxPort  m
  putWord64le       $ bvmAddrTxSvc   m
  putLazyByteString $ bvmAddrTxIp    m
  putWord16be       $ bvmAddrTxPort  m
  putWord64le       $ bvmNonce       m
  putWord8          $ bvmUserAgentBytes m
  putLazyByteString $ bvmUserAgent   m
  putInt32le        $ bvmStartHeight m
  mapM_ put         $ bvmRelay       m

data BullPayload
  = BmpVersion BullVersionMsg
  | BmpVerack
  | BmpPing Word64
  | BmpPong Word64
  | BmpRaw ByteString
  deriving (Eq, Read, Show)

instance Pretty BullPayload where
  pretty p = case p of
    BmpVersion v -> pretty v
    BmpVerack    -> pretty "verack"
    BmpPing n    -> pretty "ping" <+> pretty n
    BmpPong n    -> pretty "pong" <+> pretty n
    BmpRaw bs    -> pretty "raw"  <+> prettyBytes bs

getBullPayload :: String -> Get BullPayload
getBullPayload commandName = case commandName of
  "version" -> BmpVersion <$> getBullVersionMsg
  "verack"  -> BmpVerack  <$  eof
  "ping"    -> BmpPing    <$> getWord64le <* eof
  "pong"    -> BmpPong    <$> getWord64le <* eof
  _         -> BmpRaw     <$> getRemainingLazyByteString

eof :: Get ()
eof = guard =<< isEmpty

toBullPayload :: BullMessage -> BullPayload
toBullPayload m = runGet (getBullPayload cmdName) $ bmPayload m
  where
    cmdName = LC.unpack $ L.takeWhile (/= 0x00) $ bmhCommandName $ bmHeader m

putBullPayload :: BullPayload -> Put
putBullPayload p = case p of
  BmpVersion v -> putBullVersionMsg v
  BmpVerack    -> return ()
  BmpPing n    -> putWord64le n
  BmpPong n    -> putWord64le n
  BmpRaw bs    -> putLazyByteString bs

-- | construct a pong message from the nonce of a ping
mkPongMsg
  :: BullNet
  -> Word64 -- ^ nonce
  -> BullMessage
mkPongMsg n nonce = BullMessage
  { bmHeader  = mkBullMessageHeader (bullStartString n) "pong" payload
  , bmPayload = payload
  }
  where
    payload = runPut $ putBullPayload $ BmpPong nonce

sendPongMsg
  :: BullMessageHandle
  -> Word64 -- ^ nonce from ping
  -> IO ()
sendPongMsg hndl = sendBullMessage hndl . mkPongMsg (net hndl)

-- | verack message constructor
mkVerackMsg :: BullNet -> BullMessage
mkVerackMsg n = BullMessage
  { bmHeader  = mkBullMessageHeader (bullStartString n) "verack" mempty
  , bmPayload = mempty
  }

sendVerackMsg :: BullMessageHandle -> IO ()
sendVerackMsg hndl = sendBullMessage hndl $ mkVerackMsg $ net hndl

-- | respond to each ping with a pong
withPingPong :: BullMessageHandle -> IO a -> IO a
withPingPong hndl = fmap (either id id) . race loop
  where
    loop = recvBullMessage hndl $ \msgIO ->
      forever $ do
        payload <- toBullPayload <$> msgIO
        case payload of
          BmpPing nonce -> sendPongMsg hndl nonce
          _ -> return ()

versionHandshake :: BullMessageHandle -> IO ()
versionHandshake hndl = recvBullMessage hndl $ \msgIO -> do
  sendVersionMsg hndl
  _ <- msgIO
  sendVerackMsg hndl
  _ <- msgIO
  return ()

sendVersionMsg :: BullMessageHandle -> IO ()
sendVersionMsg hndl = sendBullMessage hndl =<< mkVersionMsg (net hndl)

mkVersionMsg :: BullNet -> IO BullMessage
mkVersionMsg n = do
  payload <- encode <$> mkVersionPayload n
  return BullMessage
    { bmHeader  = mkBullMessageHeader (bullStartString n) "version" payload
    , bmPayload = payload
    }

bullIPv6 :: BullNet -> ByteString
bullIPv6 n = L.pack $ fromIntegral <$> fromIPv6b ip
  where
    ip = case read $ bullHost n of
           IPv4 v4 -> ipv4ToIPv6 v4
           IPv6 v6 -> v6

mkVersionPayload :: BullNet -> IO BullVersionMsg
mkVersionPayload n = do
  CTime ts <- epochTime
  return BullVersionMsg
    { bvmVersion        = 70015
    , bvmServices       = 0x00
    , bvmTimestamp      = ts
    , bvmAddrRxSvc      = 0x00
    , bvmAddrRxIp       = bullIPv6 n
    , bvmAddrRxPort     = read (bullPort n)
    , bvmAddrTxSvc      = 0x00
    , bvmAddrTxIp       = loopback
    , bvmAddrTxPort     = read (bullPort n)
    , bvmNonce          = 0
    , bvmUserAgentBytes = 0
    , bvmUserAgent      = mempty
    , bvmStartHeight    = 0
    , bvmRelay          = Nothing
    }

-- | ipv6 @::ffff:127.0.0.1@
loopback :: ByteString
loopback = L.replicate 10 0x00 <> L.pack [0xff, 0xff, 0x7f, 0x00, 0x00, 0x01]
