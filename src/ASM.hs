{-# LANGUAGE QuasiQuotes #-}
module ASM where

import Data.Text (Text)
import PyF

import AST 

emit :: AST -> Text
emit (Main statements) = emitMain statements
emit (ExprStmt expr) = emitExpr expr
emit (Block stmts) = emitBlock stmts
emit _ = undefined

emitExpr :: Expr -> Text
emitExpr (Assert condition) = emitAssert condition
emitExpr (Number i) = emitNumber i
emitExpr (Not term) = emitNot term
emitExpr (Add left right) = emitAdd left right
emitExpr (Subtract left right) = emitSubtract left right
emitExpr (Multiply left right) = emitMultiply left right
emitExpr (Divide left right) = emitDivide left right
emitExpr (Equal left right) = emitEqual left right
emitExpr (NotEqual left right) = emitNotEqual left right
emitExpr _ = undefined

emitNumber :: Integer -> Text
emitNumber i = [fmt|
  ldr r0, ={i}|]

emitNot :: Expr -> Text
emitNot term = [fmt|  {emitExpr term}
  cmp r0, #0
  moveq r0, #1
  movne r0, #0|]

emitAdd :: Expr -> Expr -> Text
emitAdd left right = [fmt|
  {emitExpr left}
  push {{r0, ip}}
  {emitExpr right}
  pop {{r1, ip}}
  add r0, r0, r1|]

emitSubtract :: Expr -> Expr -> Text
emitSubtract left right = [fmt|
  {emitExpr left}
  push {{r0, ip}}
  {emitExpr right}
  pop {{r1, ip}}
  sub r0, r0, r1|]

emitMultiply :: Expr -> Expr -> Text
emitMultiply left right = [fmt|
  {emitExpr left}
  push {{r0, ip}}
  {emitExpr right}
  pop {{r1, ip}}
  mul r0, r0, r1|]

emitDivide :: Expr -> Expr -> Text
emitDivide left right = [fmt|
  {emitExpr left}
  push {{r0, ip}}
  {emitExpr right}
  pop {{r1, ip}}
  udiv r0, r0, r1|]

emitEqual :: Expr -> Expr -> Text
emitEqual left right = [fmt|
  {emitExpr left}
  push {{r0, ip}}
  {emitExpr right}
  pop {{r1, ip}}
  cmp r0, r1
  moveq r0, #1
  movne r0, #0|]

emitNotEqual :: Expr -> Expr -> Text
emitNotEqual left right = [fmt|
  {emitExpr left}
  push {{r0, ip}}
  {emitExpr right}
  pop {{r1, ip}}
  cmp r0, r1 
  moveq r0, #0
  movne r0, #1|]

emitMain :: [AST] -> Text
emitMain statements = [fmt|
.global main
main:
  push {{fp, lr}}
{foldMap emit statements}
  mov r0, #0
  pop {{fp, pc}}
|]

emitAssert :: Expr -> Text
emitAssert condition = [fmt|  {emitExpr condition}
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
|]

emitBlock :: [AST] -> Text
emitBlock stmts = foldMap emit stmts
