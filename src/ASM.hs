{-# LANGUAGE QuasiQuotes #-}
module ASM where

import Data.Text (Text)
import PyF

import AST 

emit :: AST -> Text
emit (Number i) = emitNumber i
emit (Main statements) = emitMain statements
emit (Assert condition) = emitAssert condition
emit _ = undefined

emitNumber :: Integer -> Text
emitNumber i = [fmt|
  ldr r0, ={i}
|]

emitMain :: [AST] -> Text
emitMain statements = [fmt|
.global main
main:
  push {{fp, lr}}
  {mconcat $ fmap emit statements}

  mov r0, #0
  pop {{fp, pc}}
|]

emitAssert :: AST -> Text
emitAssert condition = [fmt|
  {emit condition}
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
|]
