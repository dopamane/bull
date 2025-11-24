module Test.Bull.Message
  ( test
  ) where

import qualified Test.Bull.Message.CompactSize as CompactSize
import Test.Tasty

test :: TestTree
test = testGroup "Message"
  [ CompactSize.test
  ]
