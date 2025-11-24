module Bull.Pretty
  ( prettyBytes
  ) where

import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Numeric
import Prettyprinter

prettyBytes :: ByteString -> Doc ann
prettyBytes = foldMap renderByte . L.unpack
  where
    renderByte byt = renderNyb (byt `shiftR` 4) <> renderNyb (byt .&. 0xf)
    renderNyb  nyb = pretty $ showHex nyb ""
