{-# LANGUAGE QuasiQuotes #-}

module ParserTest where

import qualified Data.Map.Ordered.Strict as OMap
import PyF
import Test.Tasty
import Test.Tasty.HUnit

import AST
import Parser
import TestUtils

specs :: TestTree
specs =
  testGroup
    "Parser tests"
    [ testCase "Parse factorial" parseFactorialTest
    , testCase "Parse pair" parsePairTest
    , testCase "if-else condition" parseIfElseConditionTest
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
          [ Function "factorial" (Fn (OMap.fromList [("n", NumberType)]) NumberType) $
              Block
                [ Var "result" (PrimType $ Number 1)
                , While (NotEqual (Identifier "n") (PrimType $ Number 1)) $
                    Block
                      [ Assign "result" (Multiply (Identifier "result") (Identifier "n"))
                      , Assign "n" (Subtract (Identifier "n") (PrimType $ Number 1))
                      ]
                , Return (Identifier "result")
                ]
          ]
  result `assertAST` expected

parsePairTest :: Assertion
parsePairTest = do
  let result =
        parseLine
          [str|
      function pair(x: number, y: number): Array<number> {
        return [x, y];
      }
    |]
  let expected =
        Block
          [ Function
              "pair"
              (Fn{parameters = OMap.fromList [("x", NumberType), ("y", NumberType)], returnType = ArrayType NumberType})
              (Block [Return (Array [Identifier "x", Identifier "y"])])
          ]
  result `assertAST` expected

parseIfElseConditionTest :: Assertion
parseIfElseConditionTest = do
  let result =
        parseLine
          [str|
    function assert(x: boolean): void {
      if (x) {  
        putchar(46);
      } else { 
        putchar(70);
      }
    }
    |]
  let expected =
        Block
          [ Function
              "assert"
              (Fn{parameters = OMap.fromList [("x", BooleanType)], returnType = VoidType})
              ( Block
                  [ If
                      (Identifier "x")
                      (Block [ExprStmt (Call "putchar" [PrimType $ Number 46])])
                      (Block [ExprStmt (Call "putchar" [PrimType $ Number 70])])
                  ]
              )
          ]
  result `assertAST` expected
