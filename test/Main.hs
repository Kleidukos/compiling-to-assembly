{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.ByteString.Lazy (ByteString)
import PyF
import System.IO
import Test.Tasty
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit

import ASM
import AST
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (encodeUtf8)
import Parser

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  let diffCmd ref new = ["delta", "--diff-so-fancy", "--paging=never", ref, new]
  defaultMain $
    testGroup
      "Tests"
      [ testCase "Parsing" parseFactorialTest
      , goldenVsStringDiff
          "Emit empty Main"
          diffCmd
          "./test/golden/asm/empty-main.s"
          emitEmptyMainTest
      , goldenVsStringDiff
          "Emit assert"
          diffCmd
          "./test/golden/asm/assert.s"
          emitAssertTest
      , goldenVsStringDiff
          "Emit assert + negation"
          diffCmd
          "./test/golden/asm/assert-negation.s"
          emitAssertNegationTest
      , goldenVsStringDiff
          "Emit block + infix operators"
          diffCmd
          "./test/golden/asm/block-and-infix.s"
          emitBlockAndInfixTest
      , goldenVsStringDiff
          "Emit if and labels"
          diffCmd
          "./test/golden/asm/if-and-labels.s"
          emitIfAndLabelsTest
      ]

parseFactorialTest :: Assertion
parseFactorialTest = do
  let result =
        parseLine
          [str|
    function factorial(n) {
      var result = 1;
      while (n != 1) {
        result = result * n;
        n = n - 1;
      }
      return result;
    }
    |]
  let expected =
        Block
          [ Function "factorial" ["n"] $
              Block
                [ Var "result" (Number 1)
                , While (NotEqual (Identifier "n") (Number 1)) $
                    Block
                      [ Assign "result" (Multiply (Identifier "result") (Identifier "n"))
                      , Assign "n" (Subtract (Identifier "n") (Number 1))
                      ]
                , Return (Identifier "result")
                ]
          ]
  result @?= Right expected

emitEmptyMainTest :: IO ByteString
emitEmptyMainTest = do
  generated <- runCodeGen (emit (Main []))
  pure . encodeUtf8 . Text.fromStrict $ generated

emitAssertTest :: IO ByteString
emitAssertTest = do
  parsed <-
    assertRight $
      parseLine
        [str|
    function main() {
      assert(1);
    }
|]
  parsed
    @?= Block
      [ Main
          [ ExprStmt (Assert (Number 1))
          ]
      ]
  encodeUtf8 . Text.fromStrict <$> runCodeGen (emit parsed)

emitAssertNegationTest :: IO ByteString
emitAssertNegationTest = do
  parsed <-
    assertRight $
      parseLine
        [str|
    function main() {
      assert(1);
      assert(!0);
    }
|]

  parsed
    @?= Block
      [ Main
          [ ExprStmt (Assert (Number 1))
          , ExprStmt (Assert (Not (Number 0)))
          ]
      ]
  generated <- runCodeGen (emit parsed)
  pure . encodeUtf8 . Text.fromStrict $ generated

emitBlockAndInfixTest :: IO ByteString
emitBlockAndInfixTest = do
  parsed <-
    assertRight $
      parseLine
        [str|
    function main() {
      assert(1);
      assert(!0);
      assert(42 == 4 + 2 * (12 - 2) + 3 * (5 + 1));
        { /* Testing a block statement */
          assert(1);
          assert(1);
        }
    }
|]

  parsed
    @?= Block
      [ Main
          [ ExprStmt (Assert (Number 1))
          , ExprStmt (Assert (Not (Number 0)))
          , ExprStmt
              ( Assert
                  ( Equal
                      (Number 42)
                      ( Add
                          ( Add
                              (Number 4)
                              (Multiply (Number 2) (Subtract (Number 12) (Number 2)))
                          )
                          (Multiply (Number 3) (Add (Number 5) (Number 1)))
                      )
                  )
              )
          , Block
              [ ExprStmt (Assert (Number 1))
              , ExprStmt (Assert (Number 1))
              ]
          ]
      ]
  generated <- runCodeGen (emit parsed)
  pure . encodeUtf8 . Text.fromStrict $ generated

emitIfAndLabelsTest :: IO ByteString
emitIfAndLabelsTest = do
  parsed <-
    assertRight $
      parseLine
        [str|
    function main() {
      if (1) { 
        assert(1);
      } else {
        assert(0);
      }
      if (0) {
        assert(0);
      } else {
        assert(1);
      }
    }
|]

  parsed
    @?= Block
    [Main 
      [ If
          (Number 1)
          (Block [ExprStmt (Assert (Number 1))])
          (Block [ExprStmt (Assert (Number 0))])
      , If
          (Number 0)
          (Block [ExprStmt (Assert (Number 0))])
          (Block [ExprStmt (Assert (Number 1))])
      ]
    ]
  generated <- runCodeGen (emit parsed)
  pure . encodeUtf8 . Text.fromStrict $ generated

assertRight :: HasCallStack => Either b a -> IO a
assertRight (Left _a) = assertFailure "Test returned Left instead of Right"
assertRight (Right b) = pure b
