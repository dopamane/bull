module Bull.Message.Addr
  ( AddrMsg(..)
  , AddrIp(..)
  , AddrSet
  , withAddrSet
  , insertAddrMsg
  , readAddrSet
  ) where

import Bull.Message.CompactSize
import Bull.Pretty
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import Prettyprinter

-- | addr message
data AddrMsg = AddrMsg
  { addrCount :: Integer  -- ^ compact size
  , addrIps   :: [AddrIp] -- ^ address IPs
  }
  deriving (Eq, Read, Show)

instance Binary AddrMsg where
  get = getAddrMsg
  put = putAddrMsg

instance Pretty AddrMsg where
  pretty a = vsep
    [ pretty "addr:"
    , indent 2 $ vsep
      [ pretty "count:" <+> pretty (addrCount a)
      , vsep $ pretty <$> addrIps a
      ]
    ]

getAddrMsg :: Get AddrMsg
getAddrMsg = do
  c <- getCompactSize
  AddrMsg c <$> replicateM (fromIntegral c) getAddrIp

putAddrMsg :: AddrMsg -> Put
putAddrMsg m = do
  putCompactSize  $ addrCount m
  mapM_ putAddrIp $ addrIps m

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
  pretty i = vsep
    [ pretty "IP:"
    , indent 2 $ vsep
      [ pretty "time:    " <+> pretty (addrIpTime i)
      , pretty "services:" <+> pretty (addrIpSvcs i)
      , pretty "addr:    " <+> prettyBytes (addrIpAddr i)
      , pretty "port:    " <+> pretty (addrIpPort i)
      ]
    ]

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

-- | IP address tracker
data AddrSet =
  AddrSet
    Int        -- max size
    (TVar Int) -- cur len
    (TVar [AddrIp])

newAddrSet
  :: Int -- ^ max size
  -> IO AddrSet
newAddrSet s = AddrSet s <$> newTVarIO 0 <*> newTVarIO []

withAddrSet
  :: Int -- ^ max size
  -> (AddrSet -> IO a)
  -> IO a
withAddrSet s k = do
  hndl <- newAddrSet s
  either id id <$> race (monitorAddrSet hndl) (k hndl)

insertAddrMsg :: AddrSet -> AddrMsg -> IO ()
insertAddrMsg (AddrSet _ len set) msg = atomically $ do
  modifyTVar' set (addrIps msg <>)
  modifyTVar' len (+ fromIntegral (addrCount msg))

readAddrSet :: AddrSet -> IO [AddrIp]
readAddrSet (AddrSet _ _ set) = readTVarIO set

-- | when length greater than or equal to max size, halve the set
monitorAddrSet :: AddrSet -> IO a
monitorAddrSet (AddrSet siz len set) = forever $ atomically $ do
  check . (siz <=) =<< readTVar len
  modifyTVar set $ take $ siz `div` 2
  writeTVar len . length =<< readTVar set
