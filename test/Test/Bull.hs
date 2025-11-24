module Test.Bull
  ( test
  ) where

import qualified Test.Bull.Message
import Test.Tasty

test :: TestTree
test = testGroup "Bull"
  [ Test.Bull.Message.test
  ]
