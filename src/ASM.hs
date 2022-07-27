{-# LANGUAGE QuasiQuotes #-}
module ASM where

import Control.Concurrent.MVar (MVar)
import Data.Foldable (foldMap')
import Data.Text (Text)
import Data.Text.Display 
import PyF
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Text as Text

import AST

data CodeGenEnv = CodeGenEnv
  { labelCounter :: Word
  }
  deriving stock (Eq, Ord, Show)

getNextLabel :: MVar CodeGenEnv -> Text
getNextLabel envMVar = unsafePerformIO $ MVar.modifyMVar envMVar (\CodeGenEnv{labelCounter=lc} -> pure (CodeGenEnv{labelCounter = lc + 1}, Text.pack (".L" <> show lc)))
{-# NOINLINE getNextLabel #-}

newCodeGenEnv :: MVar CodeGenEnv 
newCodeGenEnv = unsafePerformIO $ MVar.newMVar (CodeGenEnv 0)
{-# NOINLINE newCodeGenEnv #-}

emit :: MVar CodeGenEnv -> AST -> Text
emit env (Main statements) = emitMain env statements
emit _env (ExprStmt expr) = emitExpr expr
emit env (Block stmts) = emitBlock env stmts
emit env (If condition consequence alternative) = emitIf env condition consequence alternative
emit _ _ = undefined

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
emitExpr (Call callee arguments) = emitCall callee arguments
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

emitCall :: Text -> [Expr] -> Text
emitCall callee arguments
  | null arguments = [fmt|  bl {callee}|]
  | length arguments == 1 = emitCall1 callee (head arguments)
  | length arguments >= 2 && length arguments <= 4 = emitCallN callee arguments
  | otherwise = error "More than 4 arguments are not supported!"

emitCall1 :: Text -> Expr -> Text
emitCall1 callee argument = [fmt|
  { emitExpr argument }
  bl {callee}
|]

-- | This function handles 4 arguments
-- We compute the memory address of current stack pointer + 16 bytes (4 words)
-- So that the next four words we push fill this space on the stack
emitCallN :: Text -> [Expr] -> Text
emitCallN callee arguments = [fmt|
  sub sp, sp, #16
  { emitCallArguments arguments }
  pop {{r0, r1, r2, r3}}
  bl {callee}
|]

emitCallArguments :: [Expr] -> Text
emitCallArguments arguments = Text.unlines $ ifor arguments $ \index arg -> foldMap' (<> "\n")
  [ emitExpr arg
  , "  str r0, [sp, #" <> display (4 * index) <> "]"
  ]

emitMain :: MVar CodeGenEnv -> [AST] -> Text
emitMain env statements = [fmt|
.global main
main:
  push {{fp, lr}}
{foldMap (emit env) statements}
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

emitBlock :: MVar CodeGenEnv -> [AST] -> Text
emitBlock env stmts = foldMap (emit env) stmts

ifor :: [a] -> (Int -> a -> b) -> [b]
ifor list fun = go ilist
  where
    ilist = zip [0..] list
    go [] = []
    go ((index, x):xs) = fun index x : go xs

emitIf :: MVar CodeGenEnv -> Expr -> AST -> AST -> Text
emitIf env conditional consequence alternative = [fmt|
  // conditional
  {emitExpr conditional}
  cmp r0, #0
  // branch to alternative
  beq {ifFalseLabel}

  // consequence
  {emit env consequence}
  b {endIfLabel}

//alternative
{ifFalseLabel}:
{emit env alternative}

{endIfLabel}:
|]
  where
    ifFalseLabel = getNextLabel env
    endIfLabel = getNextLabel env
