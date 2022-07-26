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
    [ testCase "Parsing" parseFactorialTest
    , testCase "Emit empty Main" emitEmptyMainTest
    , testCase "Emit assert" emitAssertTest
    , testCase "Emit assert + negation" emitAssertNegationTest
    , testCase "Emit block + infix operators" emitBlockAndInfixTest
    ]


parseFactorialTest :: Assertion
parseFactorialTest = do
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

emitEmptyMainTest :: Assertion
emitEmptyMainTest = do
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
  parsed <- assertRight $ parseLine [str|
    function main() {
      assert(1);
    }
|]
  parsed @?= Block
      [ Main
        [ ExprStmt (Assert (Number 1))
        ]

      ]
  let emitted = emit parsed
  emitted @?= [str|
.global main
main:
  push {fp, lr}
  
  ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar

  mov r0, #0
  pop {fp, pc}
|]

emitAssertNegationTest :: Assertion
emitAssertNegationTest = do
  parsed <- assertRight $ parseLine [str|
    function main() {
      assert(1);
      assert(!0);
    }
|]

  parsed @?= Block
     [ Main
       [ ExprStmt (Assert (Number 1))
       , ExprStmt (Assert (Not (Number 0)))
       ]
     ]
  let emitted = emit parsed
  emitted @?= [str|
.global main
main:
  push {fp, lr}
  
  ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
    
  ldr r0, =0
  cmp r0, #0
  moveq r0, #1
  movne r0, #0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar

  mov r0, #0
  pop {fp, pc}
|]

emitBlockAndInfixTest :: Assertion
emitBlockAndInfixTest = do
  parsed <- assertRight $ parseLine [str|
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

  parsed @?= Block
    [Main
      [ ExprStmt (Assert (Number 1))
      , ExprStmt (Assert (Not (Number 0)))
      , ExprStmt (Assert (
          Equal (Number 42)
                (Add
                  (Add (Number 4)
                      (Multiply (Number 2) (Subtract (Number 12) (Number 2))))
                  (Multiply (Number 3) (Add (Number 5) (Number 1))))))
      , Block
          [ ExprStmt (Assert (Number 1))
          , ExprStmt (Assert (Number 1))
          ]
      ]
    ]
  let emitted = emit parsed
  emitted @?= [str|
.global main
main:
  push {fp, lr}
  
  ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
    
  ldr r0, =0
  cmp r0, #0
  moveq r0, #1
  movne r0, #0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
  
  
  ldr r0, =42
  push {r0, ip}
  
  
  
  ldr r0, =4
  push {r0, ip}
  
  
  ldr r0, =2
  push {r0, ip}
  
  
  ldr r0, =12
  push {r0, ip}
  
  ldr r0, =2
  pop {r1, ip}
  sub r0, r0, r1
  pop {r1, ip}
  mul r0, r0, r1
  pop {r1, ip}
  add r0, r0, r1
  push {r0, ip}
  
  
  ldr r0, =3
  push {r0, ip}
  
  
  ldr r0, =5
  push {r0, ip}
  
  ldr r0, =1
  pop {r1, ip}
  add r0, r0, r1
  pop {r1, ip}
  mul r0, r0, r1
  pop {r1, ip}
  add r0, r0, r1
  pop {r1, ip}
  cmp r0, r1
  moveq r0, #1
  movne r0, #0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
  
  ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
  
  ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar

  mov r0, #0
  pop {fp, pc}
|]


assertRight :: HasCallStack => Either b a -> IO a
assertRight (Left _a) = assertFailure "Test returned Left instead of Right"
assertRight (Right b) = pure b


