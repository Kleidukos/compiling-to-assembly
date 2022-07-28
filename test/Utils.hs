module Utils where

import AST
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Stack (HasCallStack)
import System.IO
import Test.Tasty.HUnit (assertFailure, (@?=))

assertParserRight :: HasCallStack => Either String a -> IO a
assertParserRight (Right b) = pure b
assertParserRight (Left a) = do
  say a
  assertFailure "Test returned Left instead of Right"

assertAST :: HasCallStack => Either String AST -> AST -> IO ()
assertAST (Right result) expected = result @?= expected
assertAST (Left err) _ = do
  say err
  assertFailure "Test returned Left instead of Right"

say :: String -> IO ()
say = BS.hPutStrLn stdout . encodeUtf8 . Text.pack
