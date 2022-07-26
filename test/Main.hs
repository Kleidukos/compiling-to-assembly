{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO
import PyF

import Parser
import AST
import ASM (emit)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  defaultMain $ testGroup "Tests"
    [ testCase "Parsing" parseTest
    , testCase "Emit ASM" emitTest
    , testCase "Emit assert" emitAssertTest
    ]


parseTest :: Assertion
parseTest = do
  let result = parseLine [str|
    function factorial(n) {
      var result = 1;
      while (n != 1) {
        result = result * n;
        n = n - 1;
      }
      return result;
    }
    |]
  let expected = Block
        [ Function "factorial" ["n"] $ Block
          [ Var "result" (Number 1)
          , While (NotEqual (Identifier "n") (Number 1)) $ Block
            [ Assign "result" (Multiply (Identifier "result") (Identifier "n"))
            , Assign "n" (Subtract (Identifier "n") (Number 1))
            ]
          , Return (Identifier "result")
          ]
        ]
  result @?= Right expected

emitTest :: Assertion
emitTest = do
      let result = emit (Main [])
      let expected = [str|
.global main
main:
  push {fp, lr}
  

  mov r0, #0
  pop {fp, pc}
|]
      result @?= expected

emitAssertTest :: Assertion
emitAssertTest = do
  let parsed = parseLine [str|
    function main() {
      assert(1);
    }
|]
  parsed @?= Right (Main [Assert (Number 1)])
