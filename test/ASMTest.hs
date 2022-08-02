{-# LANGUAGE QuasiQuotes #-}

module ASMTest where

import Data.ByteString.Lazy (ByteString)
import PyF
import Test.Tasty
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit

import ASM
import AST
import qualified Data.Map.Ordered.Strict as OMap
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Parser
import TestUtils

diffCmd :: String -> String -> [String]
diffCmd ref new = ["delta", "--diff-so-fancy", "--paging=never", ref, new]

specs :: TestTree
specs =
  testGroup
    "ASM Tests"
    [ goldenVsStringDiff
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
    , goldenVsStringDiff
        "Emit Var"
        diffCmd
        "./test/golden/asm/var.s"
        emitVarTest
    , goldenVsStringDiff
        "While"
        diffCmd
        "./test/golden/asm/while.s"
        emitWhileTest
    ]

emitEmptyMainTest :: IO ByteString
emitEmptyMainTest = do
  generated <- runCodeGen (emit (Function "main" (Fn (OMap.fromList []) NumberType) $ Block []))
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

emitAssertTest :: IO ByteString
emitAssertTest = do
  parsed <-
    assertParserRight $
      parseLine
        [str|
    function assert(x: boolean): void {
      if (x) {  
        putchar(46);
      } else { 
        putchar(70);
      }
    }

    function main(): void {
      assert(1);
    }
|]
  parsed
    @?= Block
      [ Function
          "assert"
          (Fn{parameters = OMap.fromList [("x", BooleanType)], returnType = VoidType})
          ( Block
              [ If
                  (Identifier "x")
                  (Block [ExprStmt (Call "putchar" [PrimType (Number 46)])])
                  (Block [ExprStmt (Call "putchar" [PrimType (Number 70)])])
              ]
          )
      , Function
          "main"
          (Fn{parameters = OMap.fromList [("", NumberType)], returnType = VoidType})
          (Block [ExprStmt (Call "assert" [PrimType (Number 1)])])
      ]

  Text.encodeUtf8 . Text.fromStrict <$> runCodeGen (emit parsed)

emitAssertNegationTest :: IO ByteString
emitAssertNegationTest = do
  parsed <-
    assertParserRight $
      parseLine
        [str|
    function assert(x: boolean): void {
      if (x) {  
        putchar(46);
      } else { 
        putchar(70);
      }
    }

    function main(): void {
      assert(1);
      assert(!0);
    }
|]

  parsed
    @?= Block
      [ Function
          "assert"
          (Fn{parameters = OMap.fromList [("x", BooleanType)], returnType = VoidType})
          ( Block
              [ If
                  (Identifier "x")
                  (Block [ExprStmt (Call "putchar" [PrimType (Number 46)])])
                  (Block [ExprStmt (Call "putchar" [PrimType (Number 70)])])
              ]
          )
      , Function
          "main"
          (Fn{parameters = OMap.fromList [("", NumberType)], returnType = VoidType})
          (Block [ExprStmt (Call "assert" [PrimType (Number 1)]), ExprStmt (Call "assert" [Not (PrimType (Number 0))])])
      ]

  generated <- runCodeGen (emit parsed)
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

emitBlockAndInfixTest :: IO ByteString
emitBlockAndInfixTest = do
  parsed <-
    assertParserRight $
      parseLine
        [str|
    function assert(x: boolean): void {
      if (x) {  
        putchar(46);
      } else { 
        putchar(70);
      }
    }

    function main(): void {
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
      [ Function
          "assert"
          (Fn{parameters = OMap.fromList [("x", BooleanType)], returnType = VoidType})
          ( Block
              [ If
                  (Identifier "x")
                  (Block [ExprStmt (Call "putchar" [PrimType (Number 46)])])
                  (Block [ExprStmt (Call "putchar" [PrimType (Number 70)])])
              ]
          )
      , Function
          "main"
          (Fn{parameters = OMap.fromList [("", NumberType)], returnType = VoidType})
          ( Block
              [ ExprStmt (Call "assert" [PrimType (Number 1)])
              , ExprStmt (Call "assert" [Not (PrimType (Number 0))])
              , ExprStmt
                  ( Call
                      "assert"
                      [ Equal
                          (PrimType (Number 42))
                          ( Add
                              ( Add
                                  (PrimType (Number 4))
                                  (Multiply (PrimType (Number 2)) (Subtract (PrimType (Number 12)) (PrimType (Number 2))))
                              )
                              ( Multiply
                                  (PrimType (Number 3))
                                  ( Add
                                      (PrimType (Number 5))
                                      (PrimType (Number 1))
                                  )
                              )
                          )
                      ]
                  )
              , Block [ExprStmt (Call "assert" [PrimType (Number 1)]), ExprStmt (Call "assert" [PrimType (Number 1)])]
              ]
          )
      ]
  generated <- runCodeGen (emit parsed)
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

emitIfAndLabelsTest :: IO ByteString
emitIfAndLabelsTest = do
  parsed <-
    assertParserRight $
      parseLine
        [str|
    function assert(x: boolean): void {
      if (x) {  
        putchar(46);
      } else { 
        putchar(70);
      }
    }

    function main(): void {
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
      [ Function
          "assert"
          (Fn{parameters = OMap.fromList [("x", BooleanType)], returnType = VoidType})
          ( Block
              [ If
                  (Identifier "x")
                  (Block [ExprStmt (Call "putchar" [PrimType (Number 46)])])
                  (Block [ExprStmt (Call "putchar" [PrimType (Number 70)])])
              ]
          )
      , Function
          "main"
          (Fn{parameters = OMap.fromList [("", NumberType)], returnType = VoidType})
          ( Block
              [ If
                  (PrimType (Number 1))
                  (Block [ExprStmt (Call "assert" [PrimType (Number 1)])])
                  (Block [ExprStmt (Call "assert" [PrimType (Number 0)])])
              , If
                  (PrimType (Number 0))
                  (Block [ExprStmt (Call "assert" [PrimType (Number 0)])])
                  (Block [ExprStmt (Call "assert" [PrimType (Number 1)])])
              ]
          )
      ]
  generated <- runCodeGen (emit parsed)
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

emitVarTest :: IO ByteString
emitVarTest = do
  parsed <-
    assertParserRight $
      parseLine
        [str|
    function assert(x: boolean): void {
      if (x) {  
        putchar(46);
      } else { 
        putchar(70);
      }
    }

    function main(): void {
      var x = 4 + 2 * (12 - 2);
      var y = 3 * (5 + 1);
      var z = x + y;
      assert(z == 42);
    }
|]
  parsed
    @?= Block
      [ Function
          "assert"
          (Fn{parameters = OMap.fromList [("x", BooleanType)], returnType = VoidType})
          ( Block
              [ If
                  (Identifier "x")
                  (Block [ExprStmt (Call "putchar" [PrimType (Number 46)])])
                  (Block [ExprStmt (Call "putchar" [PrimType (Number 70)])])
              ]
          )
      , Function
          "main"
          (Fn{parameters = OMap.fromList [("", NumberType)], returnType = VoidType})
          ( Block
              [ Var "x" (Add (PrimType (Number 4)) (Multiply (PrimType (Number 2)) (Subtract (PrimType (Number 12)) (PrimType (Number 2)))))
              , Var "y" (Multiply (PrimType (Number 3)) (Add (PrimType (Number 5)) (PrimType (Number 1))))
              , Var "z" (Add (Identifier "x") (Identifier "y"))
              , ExprStmt (Call "assert" [Equal (Identifier "z") (PrimType (Number 42))])
              ]
          )
      ]

  generated <- runCodeGen (emit parsed)
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

emitWhileTest :: IO ByteString
emitWhileTest = do
  parsed <-
    assertParserRight $
      parseLine
        [str|
    function assert(x: boolean): void {
      if (x) {  
        putchar(46);
      } else { 
        putchar(70);
      }
    }

    function main(): void {
      var i = 0;
      while (i != 3) {
        i = i+ 1;
      }
      putchar(i);
      assert(i == 3);
    }
|]
  parsed
    @?= Block
      [ Function
          "assert"
          (Fn{parameters = OMap.fromList [("x", BooleanType)], returnType = VoidType})
          ( Block
              [ If
                  (Identifier "x")
                  (Block [ExprStmt (Call "putchar" [PrimType (Number 46)])])
                  (Block [ExprStmt (Call "putchar" [PrimType (Number 70)])])
              ]
          )
      , Function
          "main"
          (Fn{parameters = OMap.fromList [("", NumberType)], returnType = VoidType})
          ( Block
              [ Var "i" (PrimType (Number 0))
              , While
                  (NotEqual (Identifier "i") (PrimType (Number 3)))
                  (Block [Assign "i" (Add (Identifier "i") (PrimType (Number 1)))])
              , ExprStmt (Call "putchar" [Identifier "i"])
              , ExprStmt (Call "assert" [Equal (Identifier "i") (PrimType (Number 3))])
              ]
          )
      ]
  generated <- runCodeGen (emit parsed)
  pure . Text.encodeUtf8 . Text.fromStrict $ generated
