module Test.Bull.Message.CompactSize
  ( test
  ) where

import Bull.Message.CompactSize
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as L
import Test.Tasty
import Test.Tasty.HUnit

test :: TestTree
test = testGroup "CompactSize"
  [ getCompactSizeTest
  , putCompactSizeTest
  ]

getCompactSizeTest :: TestTree
getCompactSizeTest = testCase "getCompactSize" $ do
  decode' [0x00] @?= 0
  decode' [0xfc] @?= 252
  decode' [0xfe, 0x65, 0x97, 0x54, 0x00] @?= 5543781
  where
    decode' = runGet getCompactSize . L.pack

putCompactSizeTest :: TestTree
putCompactSizeTest = testCase "putCompactSize" $ do
  encode' 0       @?= [0x00]
  encode' 252     @?= [0xfc]
  encode' 5543781 @?= [0xfe, 0x65, 0x97, 0x54, 0x00]
  where
    encode' = L.unpack . runPut . putCompactSize
