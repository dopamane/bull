module Bull.Net
  ( BullNet(..)
  , mainnet
  , testnet
  , netIPv6
  ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.IP

-- | Network connection descriptor
data BullNet = BullNet
  { netHost        :: String     -- ^ host address
  , netPort        :: String     -- ^ port number
  , netStartString :: ByteString -- ^ net start string
  }
  deriving (Eq, Read, Show)

-- | Bitcoin mainnet
mainnet :: String -> BullNet
mainnet host       = BullNet
  { netHost        = host
  , netPort        = "8333"
  , netStartString = mainnetStartString
  }

-- | Bitcoin testnet
testnet :: String -> BullNet
testnet host       = BullNet
  { netHost        = host
  , netPort        = "18333"
  , netStartString = testnetStartString
  }

mainnetStartString :: ByteString
mainnetStartString = L.pack [0xF9, 0xBE, 0xB4, 0xD9]

testnetStartString :: ByteString
testnetStartString = L.pack [0x0B, 0x11, 0x09, 0x07]

-- | Convert host address to IPv6 address then serialize to bytestring big-endian
netIPv6 :: BullNet -> ByteString
netIPv6 n = L.pack $ fromIntegral <$> fromIPv6b ip
  where
    ip = case read $ netHost n of
           IPv4 v4 -> ipv4ToIPv6 v4
           IPv6 v6 -> v6
