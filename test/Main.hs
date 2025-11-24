module Main (main) where

import qualified Test.Bull
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Test" [Test.Bull.test]
