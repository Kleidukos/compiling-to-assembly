module TestUtils where

import AST
import Utils

import GHC.Stack (HasCallStack)
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
