module Bull.Message.CompactSize
  ( getCompactSize
  , putCompactSize
  ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

getCompactSize :: Get Integer
getCompactSize = do
  b <- getWord8
  case b of
    0xFD -> fromIntegral <$> getWord16le
    0xFE -> fromIntegral <$> getWord32le
    0xFF -> fromIntegral <$> getWord64le
    _    -> return $ fromIntegral b

putCompactSize :: Integer -> Put
putCompactSize n
  | 0          <= n && n <= 252                  = putWord8    $ fromIntegral n
  | 253        <= n && n <= 65535                = putWord8 0xFD <> putWord16le (fromIntegral n)
  | 65536      <= n && n <= 4294967295           = putWord8 0xFE <> putWord32le (fromIntegral n)
  | 4294967296 <= n && n <= 18446744073709551615 = putWord8 0xFF <> putWord64le (fromIntegral n)
  | otherwise                                    = error "size out of range"
