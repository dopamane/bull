{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Bull.Net
  ( Net(..)
  , mainnet
  , testnet
  , netIPv6
  ) where

import Bull.Pretty
import Data.Binary
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Hashable
import Data.IP
import GHC.Generics
import Prettyprinter

-- | Network connection descriptor
data Net = Net
  { netHost        :: String     -- ^ host address
  , netPort        :: String     -- ^ port number
  , netStartString :: ByteString -- ^ net start string
  }
  deriving (Binary, Eq, Generic, Hashable, Read, Show)

instance Pretty Net where
  pretty n = vsep
    [ pretty "net:"
    , indent 2 $ vsep
      [ pretty "host: " <+> pretty (netHost n)
      , pretty "port: " <+> pretty (netPort n)
      , pretty "start:" <+> prettyBytes (netStartString n)
      ]
    ]

-- | Bitcoin mainnet
mainnet :: String -> Net
mainnet host       = Net
  { netHost        = host
  , netPort        = "8333"
  , netStartString = mainnetStartString
  }

-- | Bitcoin testnet
testnet :: String -> Net
testnet host       = Net
  { netHost        = host
  , netPort        = "18333"
  , netStartString = testnetStartString
  }

mainnetStartString :: ByteString
mainnetStartString = L.pack [0xF9, 0xBE, 0xB4, 0xD9]

testnetStartString :: ByteString
testnetStartString = L.pack [0x0B, 0x11, 0x09, 0x07]

-- | Convert host address to IPv6 address then serialize to bytestring big-endian
netIPv6 :: Net -> ByteString
netIPv6 n = L.pack $ fromIntegral <$> fromIPv6b ip
  where
    ip = case read $ netHost n of
           IPv4 v4 -> ipv4ToIPv6 v4
           IPv6 v6 -> v6
