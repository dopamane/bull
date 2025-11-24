module Bull.Message.Version
  ( BullVersionMsg(..)
  ) where

import Bull.Message.CompactSize
import Bull.Pretty
import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Int
import Prettyprinter

-- | Version message
data BullVersionMsg = BullVersionMsg
  { bvmVersion        :: Int32 -- ^ protocol
  , bvmServices       :: Word64
  , bvmTimestamp      :: Int64
  , bvmAddrRxSvc      :: Word64
  , bvmAddrRxIp       :: ByteString
  , bvmAddrRxPort     :: Word16
  , bvmAddrTxSvc      :: Word64
  , bvmAddrTxIp       :: ByteString
  , bvmAddrTxPort     :: Word16
  , bvmNonce          :: Word64
  , bvmUserAgentBytes :: Integer -- ^ compact size
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
  userAgentBytes <- getCompactSize
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
  putCompactSize    $ bvmUserAgentBytes m
  putLazyByteString $ bvmUserAgent   m
  putInt32le        $ bvmStartHeight m
  mapM_ put         $ bvmRelay       m
