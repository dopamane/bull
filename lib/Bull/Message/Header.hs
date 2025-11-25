module Bull.Message.Header
  ( MsgHdr(..)
  , mkMsgHdr
  , getHeader
  , putHeader
  ) where

import Bull.Net
import Bull.Pretty
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Digest.Pure.SHA
import Data.Function
import Prettyprinter

data MsgHdr = MsgHdr
  { bmhStartString :: ByteString
  , bmhCommandName :: ByteString
  , bmhPayloadSize :: Word32
  , bmhChecksum    :: ByteString
  }
  deriving (Eq, Read, Show)

instance Pretty MsgHdr where
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
mkMsgHdr
  :: ByteString -- ^ start string 'mainnetStartString'
  -> String     -- ^ command name
  -> ByteString -- ^ payload
  -> MsgHdr
mkMsgHdr startString commandName payload = MsgHdr
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

renderCommandName :: MsgHdr -> Doc ann
renderCommandName =
  pretty . LC.unpack . L.takeWhile (/= 0x00) . bmhCommandName

getHeader :: BullNet -> Get MsgHdr
getHeader n =
  MsgHdr
    <$> getStartString n
    <*> getLazyByteString 12
    <*> getWord32le
    <*> getLazyByteString 4

-- | validate start string
getStartString :: BullNet -> Get ByteString
getStartString n = do
  s <- getLazyByteString 4
  when (s /= netStartString n) $ fail "invalid start string"
  return s

putHeader :: MsgHdr -> Put
putHeader hdr = do
  putLazyByteString $ bmhStartString hdr
  putLazyByteString $ bmhCommandName hdr
  putWord32le       $ bmhPayloadSize hdr
  putLazyByteString $ bmhChecksum    hdr
