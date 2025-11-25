module Bull.Message.Addr
  ( AddrMsg(..)
  , AddrIp(..)
  ) where

import Bull.Message.CompactSize
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import Prettyprinter

-- | addr message
data AddrMsg = AddrMsg
  { addrCount :: Integer  -- ^ compact size
  , addrs     :: [AddrIp] -- ^ address IPs
  }
  deriving (Eq, Read, Show)

instance Binary AddrMsg where
  get = getAddrMsg
  put = putAddrMsg

instance Pretty AddrMsg where
  pretty = viaShow

getAddrMsg :: Get AddrMsg
getAddrMsg = do
  c <- getCompactSize
  AddrMsg c <$> replicateM (fromIntegral c) getAddrIp

putAddrMsg :: AddrMsg -> Put
putAddrMsg m = do
  putCompactSize  $ addrCount m
  mapM_ putAddrIp $ addrs m

data AddrIp = AddrIp
  { addrIpTime :: Word32     -- ^ timestamp
  , addrIpSvcs :: Word64     -- ^ services
  , addrIpAddr :: ByteString -- ^ IP address
  , addrIpPort :: Word16     -- ^ port
  }
  deriving (Eq, Read, Show)

instance Binary AddrIp where
  get = getAddrIp
  put = putAddrIp

instance Pretty AddrIp where
  pretty = viaShow

getAddrIp :: Get AddrIp
getAddrIp =
  AddrIp
    <$> getWord32le
    <*> getWord64le
    <*> getLazyByteString 16
    <*> getWord16be

putAddrIp :: AddrIp -> Put
putAddrIp i = do
  putWord32le       $ addrIpTime i
  putWord64le       $ addrIpSvcs i
  putLazyByteString $ addrIpAddr i
  putWord16be       $ addrIpPort i
