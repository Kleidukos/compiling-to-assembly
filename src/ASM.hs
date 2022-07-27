{-# LANGUAGE QuasiQuotes #-}

{- |

   1 ~ True
   0 ~ False

|
-}
module ASM where

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Foldable (foldMap')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Display
import PyF

import AST

data CodeGenEnv = CodeGenEnv
  { labelCounter :: Word
  , locals :: Map Text Int
  }
  deriving stock (Eq, Ord, Show)

type CodeGenM a = ReaderT (MVar CodeGenEnv) IO a

getNextLabel :: CodeGenM Text
getNextLabel = do
  envMVar <- ask
  liftIO $
    MVar.modifyMVar
      envMVar
      ( \CodeGenEnv{labelCounter = lc, ..} ->
          pure (CodeGenEnv{labelCounter = lc + 1, ..}, Text.pack (".L" <> show lc))
      )

newCodeGenEnv :: IO (MVar CodeGenEnv)
newCodeGenEnv = MVar.newMVar (CodeGenEnv{labelCounter = 0, locals = Map.empty})

runCodeGen :: CodeGenM Text -> IO Text
runCodeGen computation = do
  env <- newCodeGenEnv
  runReaderT computation env

emit :: AST -> CodeGenM Text
emit (ExprStmt expr) = emitExpr expr
emit (Block stmts) = emitBlock stmts
emit (If condition consequence alternative) = emitIf condition consequence alternative
emit (Function name arguments body) = emitFunction name arguments body
emit (Return body) = emitReturn body
emit _ = undefined

emitExpr :: Expr -> CodeGenM Text
emitExpr (Number i) = emitNumber i
emitExpr (Not term) = emitNot term
emitExpr (Add left right) = emitAdd left right
emitExpr (Subtract left right) = emitSubtract left right
emitExpr (Multiply left right) = emitMultiply left right
emitExpr (Divide left right) = emitDivide left right
emitExpr (Equal left right) = emitEqual left right
emitExpr (NotEqual left right) = emitNotEqual left right
emitExpr (Call callee arguments) = emitCall callee arguments
emitExpr (Identifier identifier) = emitIdentifier identifier

emitNumber :: Integer -> CodeGenM Text
emitNumber i =
  pure [fmt|ldr r0, ={i}|]

emitNot :: Expr -> CodeGenM Text
emitNot term = do
  renderedTerm <- emitExpr term
  pure
    [fmt|
  {renderedTerm}
  cmp r0, #0
  moveq r0, #1
  movne r0, #0|]

emitAdd :: Expr -> Expr -> CodeGenM Text
emitAdd left right = do
  leftExpr <- emitExpr left
  rightExpr <- emitExpr right
  pure
    [fmt|
  {leftExpr}
  push {{r0, ip}}
  {rightExpr}
  pop {{r1, ip}}
  add r0, r0, r1|]

emitSubtract :: Expr -> Expr -> CodeGenM Text
emitSubtract left right = do
  leftExpr <- emitExpr left
  rightExpr <- emitExpr right
  pure
    [fmt|
  {leftExpr}
  push {{r0, ip}}
  {rightExpr}
  pop {{r1, ip}}
  sub r0, r0, r1|]

emitMultiply :: Expr -> Expr -> CodeGenM Text
emitMultiply left right = do
  leftExpr <- emitExpr left
  rightExpr <- emitExpr right
  pure
    [fmt|
  {leftExpr}
  push {{r0, ip}}
  {rightExpr}
  pop {{r1, ip}}
  mul r0, r0, r1|]

emitDivide :: Expr -> Expr -> CodeGenM Text
emitDivide left right = do
  leftExpr <- emitExpr left
  rightExpr <- emitExpr right
  pure
    [fmt|
  {leftExpr}
  push {{r0, ip}}
  {rightExpr}
  pop {{r1, ip}}
  udiv r0, r0, r1|]

emitEqual :: Expr -> Expr -> CodeGenM Text
emitEqual left right = do
  leftExpr <- emitExpr left
  rightExpr <- emitExpr right
  pure
    [fmt|
  {leftExpr}
  push {{r0, ip}}
  {rightExpr}
  pop {{r1, ip}}
  cmp r0, r1
  moveq r0, #1
  movne r0, #0|]

emitNotEqual :: Expr -> Expr -> CodeGenM Text
emitNotEqual left right = do
  leftExpr <- emitExpr left
  rightExpr <- emitExpr right
  pure
    [fmt|
  {leftExpr}
  push {{r0, ip}}
  {rightExpr}
  pop {{r1, ip}}
  cmp r0, r1 
  moveq r0, #0
  movne r0, #1|]

emitCall :: Text -> [Expr] -> CodeGenM Text
emitCall callee arguments
  | null arguments = do
      pure [fmt|  bl {callee}|]
  | length arguments == 1 = emitCall1 callee (head arguments)
  | length arguments >= 2 && length arguments <= 4 = emitCallN callee arguments
  | otherwise = error "More than 4 arguments are not supported!"

emitCall1 :: Text -> Expr -> CodeGenM Text
emitCall1 callee argument = do
  renderedArgument <- emitExpr argument
  pure
    [fmt|
  {renderedArgument}
  bl {callee}
|]

{- | This function handles 4 arguments
 We compute the memory address of current stack pointer + 16 bytes (4 words)
 So that the next four words we push fill this space on the stack
-}
emitCallN :: Text -> [Expr] -> CodeGenM Text
emitCallN callee arguments = do
  renderedArguments <- emitCallArguments arguments
  pure
    [fmt|
  sub sp, sp, #16
  {renderedArguments}
  pop {{r0, r1, r2, r3}}
  bl {callee}
|]

emitCallArguments :: [Expr] -> CodeGenM Text
emitCallArguments arguments = do
  args <- iforM arguments $ \index arg -> do
    argExpr <- emitExpr arg
    pure $
      foldMap'
        (<> "\n")
        [ argExpr
        , "  str r0, [sp, #" <> display (4 * index) <> "]"
        ]
  pure $ Text.unlines args

emitIdentifier :: Text -> CodeGenM Text
emitIdentifier identifier = do
  env <- liftIO . MVar.readMVar =<< ask
  case Map.lookup identifier (locals env) of
    Nothing -> pure $ error "Undefined variable: " <> identifier
    Just offset -> pure [fmt|ldr r0, [fp, #{offset}]|]

emitAssert :: Expr -> CodeGenM Text
emitAssert condition = do
  conditionExpr <- emitExpr condition
  pure
    [fmt|{conditionExpr}
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
|]

emitBlock :: [AST] -> CodeGenM Text
emitBlock stmts = mconcat <$> traverse emit stmts

emitIf :: Expr -> AST -> AST -> CodeGenM Text
emitIf conditional consequence alternative = do
  ifFalseLabel <- getNextLabel
  endIfLabel <- getNextLabel
  conditionalExpr <- emitExpr conditional
  consequenceStmt <- emit consequence
  alternativeStmt <- emit alternative

  pure
    [fmt|
  // conditional
  {conditionalExpr}
  // is the conditional false?
  cmp r0, #0 
  // if yes, branch to alternative
  beq {ifFalseLabel}

  // if no, we go to the consequence
  {consequenceStmt}
  // and branch to the next block of instructions
  b {endIfLabel}

// alternative (the condition was false)
{ifFalseLabel}:
{alternativeStmt}
// end of conditional
{endIfLabel}:
|]

emitFunction :: Text -> [Text] -> AST -> CodeGenM Text
emitFunction name arguments body
  | length arguments > 4 = error "More than 4 arguments is not supported!"
  | otherwise = emitFunction4 name arguments body

emitFunction4 :: Text -> [Text] -> AST -> CodeGenM Text
emitFunction4 name arguments body = do
  let prologue = emitPrologue
  let epilogue = emitEpilogue
  newLocals <- setupNewLocals arguments
  envMVar <- ask
  liftIO $
    MVar.modifyMVar_
      envMVar
      ( \CodeGenEnv{labelCounter = lc} ->
          pure CodeGenEnv{labelCounter = lc, locals = newLocals}
      )
  renderedBody <- emit body
  pure
    [fmt|
.global {name}
{name}:
{prologue}
{renderedBody}
{epilogue}
|]

emitPrologue :: Text
emitPrologue =
  [fmt|
  push {{fp, lr}}
  mov fp, sp
  push {{r0, r1, r2, r3}}|]

emitEpilogue :: Text
emitEpilogue =
  [fmt|
  mov sp, fp
  mov r0, #0
  pop {{fp, pc}}|]

setupNewLocals :: [Text] -> CodeGenM (Map Text Int)
setupNewLocals arguments = do
  locals <- iforM arguments $ \index arg -> pure (arg, 4 * index - 16)
  pure $ Map.fromList locals

emitReturn :: Expr -> CodeGenM Text
emitReturn body = do
  renderedBody <- emitExpr body
  pure
    [fmt|
  {renderedBody}
  mov sp, fp
  pop {{fp, pc}}
|]

iforM :: [a] -> (Int -> a -> CodeGenM b) -> CodeGenM [b]
iforM list fun = go ilist
  where
    ilist = zip [0 ..] list
    go l = mapM (uncurry fun) l
