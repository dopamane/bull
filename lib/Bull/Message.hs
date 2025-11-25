module Bull.Message
  ( BullMessage(..)
  , BullPayload(..)
  , toBullPayload
  , getMessage
  , putMessage
  , pongMsg
  , versionMsg
  , verackMsg
  , getAddrMsg
  ) where

import Bull.Message.Addr
import Bull.Message.Header
import Bull.Message.Version
import Bull.Net
import Bull.Pretty
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Prettyprinter

data BullMessage = BullMessage
  { bmHeader  :: MsgHdr
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
  { bmHeader  = mkMsgHdr (netStartString n) "pong" payload
  , bmPayload = payload
  }
  where
    payload = runPut $ putBullPayload $ BmpPong nonce

-- | verack message constructor
verackMsg :: BullNet -> BullMessage
verackMsg = emptyMsg "verack"

versionMsg :: BullNet -> IO BullMessage
versionMsg n = do
  payload <- encode <$> mkVersionMsg n
  return BullMessage
    { bmHeader  = mkMsgHdr (netStartString n) "version" payload
    , bmPayload = payload
    }

getAddrMsg :: BullNet -> BullMessage
getAddrMsg = emptyMsg "getaddr"

-- | message with no payload
emptyMsg :: String -> BullNet -> BullMessage
emptyMsg msg n = BullMessage
  { bmHeader   = mkMsgHdr (netStartString n) msg mempty
  , bmPayload  = mempty
  }
