module Test.Bull.Message.Addr
  ( tests
  ) where

import Bull.Message.Addr
import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.Functor.Identity
import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Addr"
  [ binaryIdentity
  ]

binaryIdentity :: TestTree
binaryIdentity = testProperty "binary-identity" $ property $ do
  x <- forAll genAddrMsg
  tripping x encode $ Identity . decode

genAddrMsg :: Gen AddrMsg
genAddrMsg = do
  cnt <- Gen.integral $ Range.linear 0 1000
  AddrMsg cnt <$> Gen.list (Range.singleton $ fromIntegral cnt) genAddrIp

genAddrIp :: Gen AddrIp
genAddrIp =
  AddrIp
    <$> Gen.word32 Range.linearBounded
    <*> Gen.word64 Range.linearBounded
    <*> L.fromStrict `fmap` Gen.bytes (Range.singleton 16)
    <*> Gen.word16 Range.linearBounded
