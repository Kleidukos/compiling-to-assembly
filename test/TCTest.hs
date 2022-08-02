{-# LANGUAGE QuasiQuotes #-}

module TCTest where

import PyF
import Test.Tasty
import Test.Tasty.HUnit

import AST
import qualified Data.Map as Map
import Parser
import TestUtils
import TypeChecker

specs :: TestTree
specs =
  testGroup
    "Type Checker tests"
    [ testCase "addition" additionTest
    , testCase "function + boolean" boolFunctionTest
    ]

additionTest :: Assertion
additionTest = do
  let ast = Add (PrimType $ Number 2) (PrimType $ Number 4)
  (tcResult, _tcEnv) <- runTypeCheckM (checkExpr ast)
  assertEqual "Addition is well-typed" (Right NumberType) tcResult

boolFunctionTest :: Assertion
boolFunctionTest = do
  parsed <-
    assertParserRight $
      parseLine
        [str|
      function compare(x: number, y: number): boolean {
        if ((x + y) != 3) {
          return true;
        } else {
          return false;
        }
      }
      |]
  (tcResult, tcEnv) <- runTypeCheckM (checkNode parsed)
  assertEqual "Function statement is well-typed" (Right VoidType) tcResult
  assertEqual "Function itself is well-typed" (Just BooleanType) (returnType <$> Map.lookup "compare" (functions tcEnv))
