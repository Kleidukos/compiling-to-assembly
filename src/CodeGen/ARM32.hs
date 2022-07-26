{-# LANGUAGE QuasiQuotes #-}

{-
   1 ~ True
   0 ~ False
-}
module CodeGen.ARM32 where

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Foldable (foldMap')
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Ordered.Strict as OMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Display
import PyF

import AST
import Utils

data CodeGenEnv = CodeGenEnv
  { labelCounter :: Word
  , locals :: Map Text Int
  , nextLocalOffset :: Int
  }
  deriving stock (Eq, Ord, Show)

setOffset :: Text -> CodeGenM ()
setOffset name = do
  envMVar <- ask
  liftIO $
    MVar.modifyMVar_
      envMVar
      ( \CodeGenEnv{locals, nextLocalOffset, ..} -> do
          let newOffset = nextLocalOffset - 4
          let newNextLocalOffset = nextLocalOffset - 8
          let newLocals = Map.insert name newOffset locals
          pure CodeGenEnv{locals = newLocals, nextLocalOffset = newNextLocalOffset, ..}
      )

getOffset :: Text -> CodeGenM Int
getOffset name = do
  env <- liftIO . MVar.readMVar =<< ask
  case Map.lookup name (locals env) of
    Nothing -> pure $ error (Text.unpack $ "Undefined variable: " <> name)
    Just offset -> pure offset

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
newCodeGenEnv = MVar.newMVar (CodeGenEnv{labelCounter = 0, locals = Map.empty, nextLocalOffset = -20})

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
emit (Var name value) = emitVar name value
emit (While condition body) = emitWhile condition body
emit (Assign var content) = emitAssign var content

emitExpr :: Expr -> CodeGenM Text
emitExpr (Add left right) = emitAdd left right
emitExpr (Array content) = emitArray content
emitExpr (ArrayLookup array index) = emitArrayLookup array index
emitExpr (Call callee arguments) = emitCall callee arguments
emitExpr (Divide left right) = emitDivide left right
emitExpr (Equal left right) = emitEqual left right
emitExpr (Identifier identifier) = emitIdentifier identifier
emitExpr (Length expression) = emitLength expression
emitExpr (Multiply left right) = emitMultiply left right
emitExpr (Not term) = emitNot term
emitExpr (NotEqual left right) = emitNotEqual left right
emitExpr (Subtract left right) = emitSubtract left right
emitExpr (PrimType ty) = emitPrim ty

emitPrim :: PrimitiveType -> CodeGenM Text
emitPrim (Number i) = emitNumber i
emitPrim (Boolean bool) = emitBoolean bool
emitPrim Null = emitBoolean False
emitPrim Undefined = emitBoolean False

emitLength :: Expr -> CodeGenM Text
emitLength array = do
  renderedArray <- emitExpr array
  pure
    [fmt|
  {renderedArray}
  ldr r0, [r0, #0]
|]

emitArray :: [Expr] -> CodeGenM Text
emitArray elements = do
  let arrayLength = length elements
  args' <- iforM elements $ \index arg -> do
    argExpr <- emitExpr arg
    pure $
      foldMap'
        (<> "\n")
        [ argExpr
        , "  str r0, [r4, #" <> display (4 * (index + 1)) <> "]"
        ]
  let args = Text.unlines args'
  pure
    [fmt|
  // malloc enough memory for n words in bytes (1 word = 4 bytes)
  ldr r0, ={4 * (arrayLength + 1)} 
  bl malloc

  // get the pointer in r0, save it in r4,
  // but before, save the value of r4 on the stack.
  push {{r4, ip}}
  mov r4, r0

  // store array length in the first word
  // of the allocate span of memory
  ldr r0, ={arrayLength}
  // at offset 0 (implicitly)
  str r0, [r4]

  // emit code for each element
  // and store it into the corresponding memory slot.
  {args}

  // return the pointer in r0
  // and restore the call-preserved r4
  mov r0, r4
  pop {{r4, ip}}
|]

emitArrayLookup :: Expr -> Expr -> CodeGenM Text
emitArrayLookup array index = do
  renderedArray <- emitExpr array
  renderedIndex <- emitExpr index
  pure
    [fmt|
  // store the array pointer in r1
  // and array index in r0
  {renderedArray}
  push {{r0, ip}}
  {renderedIndex}
  pop {{r1, ip}}

  // perform bound check
  // and convert array index into a byte offset
  // (with lsl #2, equivalent to multiplying by 4)
  ldr r2, [r1], #4
  cmp r0, r2
  movhs r0, #0
  ldrlo r0, [r1, r0, lsl #2]
|]

emitNumber :: Integer -> CodeGenM Text
emitNumber i =
  pure [fmt|ldr r0, ={i}|]

emitBoolean :: Bool -> CodeGenM Text
emitBoolean bool = do
  if bool
    then pure [fmt|  mov r0, #1 |]
    else pure [fmt|  mov r0, #0 |]

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
  // left expression
  {leftExpr}
  push {{r0, ip}}
  // right expression
  {rightExpr}
  pop {{r1, ip}}
  // left == right ?
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
  | null arguments = pure [fmt|  bl {callee}|]
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
    Nothing -> pure $ error (Text.unpack $ "Undefined variable: " <> identifier)
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

emitFunction :: Text -> Fn -> AST -> CodeGenM Text
emitFunction name arguments body
  | OMap.size (parameters arguments) > 4 = error "More than 4 arguments is not supported!"
  | otherwise = emitFunction4 name (fmap fst $ OMap.assocs (parameters arguments)) body

emitFunction4 :: Text -> [Text] -> AST -> CodeGenM Text
emitFunction4 name arguments body = do
  let prologue = emitPrologue
  let epilogue = emitEpilogue
  newLocals <- setupNewLocals arguments
  envMVar <- ask
  liftIO $
    MVar.modifyMVar_
      envMVar
      ( \CodeGenEnv{..} ->
          pure CodeGenEnv{locals = newLocals, ..}
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

emitVar :: Text -> Expr -> CodeGenM Text
emitVar name value = do
  renderedValue <- emitExpr value
  setOffset name
  pure
    [fmt|
  {renderedValue}
  push {{r0, ip}}
|]

emitWhile :: Expr -> AST -> CodeGenM Text
emitWhile condition body = do
  loopStart <- getNextLabel
  loopEnd <- getNextLabel
  renderedCondition <- emitExpr condition
  renderedBody <- emit body
  pure
    [fmt| 
// loop start
{loopStart}:
{renderedCondition} 

  // did we reach the loop-breaking condition?
  // if yes, branch to {loopEnd}
  cmp r0, #0
  beq {loopEnd}
{renderedBody}
  b {loopStart}
// loop end
{loopEnd}:
|]

emitAssign :: Text -> Expr -> CodeGenM Text
emitAssign name value = do
  renderedValue <- emitExpr value
  offset <- getOffset name
  pure
    [fmt| 
  {renderedValue}
  str r0, [fp, #{offset}]
|]
