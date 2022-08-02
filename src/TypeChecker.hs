{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State.Strict
import qualified Control.Monad.State.Strict as State
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Map.Ordered.Strict (OMap)
import qualified Data.Map.Ordered.Strict as OMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import AST
import Data.Text.Display (display)
import Utils

data TypeCheckEnv = TypeCheckEnv
  { locals :: OMap Text TsType
  , functions :: Map Text Fn
  , currentFunctionReturnType :: Maybe TsType
  }
  deriving stock (Eq, Ord, Show)

emptyTcEnv :: TypeCheckEnv
emptyTcEnv =
  TypeCheckEnv
    { locals = OMap.empty
    , functions = Map.empty
    , currentFunctionReturnType = Nothing
    }

data TypeCheckError
  = TypeMismatch
      TsType
      -- ^ expected
      TsType
      -- ^ got
  | UndefinedVariable Text
  | UndefinedFunction Text
  | EmptyArray
  | -- | got
    NotAnArray
      TsType
  | ReturnStatementOutOfFunction
  deriving stock (Eq, Ord, Show)

newtype TypeCheckM a = TypeCheckM {getTc :: ExceptT TypeCheckError (StateT TypeCheckEnv IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadError TypeCheckError, MonadState TypeCheckEnv, MonadIO)

runTypeCheckM :: forall a. TypeCheckM a -> IO (Either TypeCheckError a, TypeCheckEnv)
runTypeCheckM computation = runStateT (runExceptT $ getTc computation) emptyTcEnv

assertType :: TsType -> TsType -> TypeCheckM ()
assertType expected got =
  when (expected /= got) $ do
    tcEnv <- get
    liftIO $ say $ show tcEnv
    throwError $ TypeMismatch expected got

checkNode :: AST -> TypeCheckM TsType
checkNode (Assign name value) = checkAssign name value
checkNode (ExprStmt expr) = checkExpr expr
checkNode (Function name signature body) = checkFunction name signature body
checkNode (If condition consequence alternative) = checkIf condition consequence alternative
checkNode (Var name value) = checkVar name value
checkNode (While condition body) = checkWhile condition body
checkNode (Return expr) = checkReturn expr
checkNode (Block exprs) = checkBlock exprs

checkPrimType :: PrimitiveType -> TypeCheckM TsType
checkPrimType (Boolean _) = pure BooleanType
checkPrimType (Number _) = pure NumberType
checkPrimType Undefined = pure VoidType
checkPrimType Null = pure VoidType

checkExpr :: Expr -> TypeCheckM TsType
checkExpr (Not booleanExpr) = checkNot booleanExpr
checkExpr (Length expr) = checkArrayLength expr
checkExpr (Call callee arguments) = checkCall callee arguments
checkExpr (Array args) = checkArray args
checkExpr (ArrayLookup arg index) = checkArrayLookup arg index
checkExpr (Identifier name) = checkId name
checkExpr (PrimType ty) = checkPrimType ty
checkExpr (Add left right) = checkNumericOp left right
checkExpr (Divide left right) = checkNumericOp left right
checkExpr (Subtract left right) = checkNumericOp left right
checkExpr (Multiply left right) = checkNumericOp left right
checkExpr (Equal left right) = checkBooleanOp left right
checkExpr (NotEqual left right) = checkBooleanOp left right

checkNot :: Expr -> TypeCheckM TsType
checkNot node = do
  checkedNode <- checkExpr node
  assertType BooleanType checkedNode
  pure BooleanType

checkNumericOp :: Expr -> Expr -> TypeCheckM TsType
checkNumericOp left right = do
  checkedLeft <- checkExpr left
  checkedRight <- checkExpr right
  assertType NumberType checkedLeft
  assertType NumberType checkedRight
  pure NumberType

checkBooleanOp :: Expr -> Expr -> TypeCheckM TsType
checkBooleanOp left right = do
  checkedLeft <- checkExpr left
  checkedRight <- checkExpr right
  assertType checkedRight checkedLeft
  pure BooleanType

checkVar :: Text -> Expr -> TypeCheckM TsType
checkVar name value = do
  varType <- checkExpr value
  addLocal name varType
  pure VoidType

checkId :: Text -> TypeCheckM TsType
checkId name = lookupLocal name

checkAssign :: Text -> Expr -> TypeCheckM TsType
checkAssign name value = do
  varType <- lookupLocal name
  valueType <- checkExpr value
  assertType varType valueType
  pure VoidType

checkArray :: [Expr] -> TypeCheckM TsType
checkArray args = do
  array <- guardThatArrayIsNonEmpty args
  (a :| argsTypes) <- traverse checkExpr array
  elementType <- foldM (\prev next -> assertType prev next >> pure prev) a argsTypes
  pure $ ArrayType elementType

checkArrayLength :: Expr -> TypeCheckM TsType
checkArrayLength arg = do
  argType <- checkExpr arg
  case argType of
    ArrayType _ -> pure NumberType
    _ -> throwError $ NotAnArray argType

checkArrayLookup :: Expr -> Expr -> TypeCheckM TsType
checkArrayLookup arg index = do
  checkedIndex <- checkExpr index
  assertType NumberType checkedIndex
  argType <- checkExpr arg
  case argType of
    ArrayType _ -> undefined
    _ -> throwError $ NotAnArray argType

checkFunction :: Text -> Fn -> AST -> TypeCheckM TsType
checkFunction name signature body = do
  addFunction name signature
  void $ checkNode body
  pure VoidType

checkCall :: Text -> [Expr] -> TypeCheckM TsType
checkCall callee arguments = do
  expected <- getFunction callee
  let (indexedArguments :: [(Word, Expr)]) = zip [0 ..] arguments
  argTypes <-
    foldM
      ( \acc (index, element) -> do
          checkedElement <- checkExpr element
          pure $ acc OMap.|> ("x" <> display index, checkedElement)
      )
      OMap.empty
      indexedArguments
  let got = FunctionType $ Fn{parameters = argTypes, returnType = returnType expected}
  assertType (FunctionType expected) got
  pure (returnType expected)

checkReturn :: Expr -> TypeCheckM TsType
checkReturn returnExpr = do
  returnType <- checkExpr returnExpr
  env <- get
  case currentFunctionReturnType env of
    Nothing -> throwError ReturnStatementOutOfFunction
    Just t -> do
      assertType t returnType
      pure VoidType

checkBlock :: [AST] -> TypeCheckM TsType
checkBlock exprs = do
  res <- mapM checkNode exprs
  pure $ last res

checkIf :: Expr -> AST -> AST -> TypeCheckM TsType
checkIf condition consequence alternative = do
  checkExpr condition
  checkNode consequence
  checkNode alternative
  pure VoidType

checkWhile :: Expr -> AST -> TypeCheckM TsType
checkWhile condition body = do
  checkExpr condition
  checkNode body
  pure VoidType

--

addFunction :: Text -> Fn -> TypeCheckM ()
addFunction name signature = do
  State.modify
    ( \env ->
        let TypeCheckEnv{functions} = env
            newFunctions = Map.insert name signature functions
         in TypeCheckEnv{functions = newFunctions, locals = parameters signature, currentFunctionReturnType = Just (returnType signature)}
    )

getFunction :: Text -> TypeCheckM Fn
getFunction name = do
  result <- State.gets (\TypeCheckEnv{functions} -> Map.lookup name functions)
  case result of
    Just a -> pure a
    Nothing -> throwError $ UndefinedFunction name

addLocal :: Text -> TsType -> TypeCheckM ()
addLocal name tsType =
  State.modify
    ( \env ->
        let TypeCheckEnv{locals} = env
            newLocals = locals OMap.|> (name, tsType)
         in env{locals = newLocals}
    )

lookupLocal :: Text -> TypeCheckM TsType
lookupLocal name = do
  result <- State.gets (\TypeCheckEnv{locals} -> OMap.lookup name locals)
  case result of
    Just a -> pure a
    Nothing -> throwError $ UndefinedVariable name

guardThatArrayIsNonEmpty :: [a] -> TypeCheckM (NonEmpty a)
guardThatArrayIsNonEmpty array =
  case NonEmpty.nonEmpty array of
    Nothing -> throwError EmptyArray
    Just neList -> pure neList
