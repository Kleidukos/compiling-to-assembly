{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker where

import Control.Monad.Except (ExceptT, MonadError, throwError)
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

data TypeCheckEnv = TypeCheckEnv
  { locals :: OMap Text TsType
  , functions :: Map Text Fn
  , currentFunctionReturnType :: Maybe TsType
  }

data TypeCheckError
  = TypeMismatch
      TsType
      -- ^ expected
      TsType
      -- ^ got
  | UndefinedVariable Text
  | EmptyArray
  | -- | got
    NotAnArray
      TsType
  | ReturnStatementOutOfFunction
  deriving stock (Eq, Ord, Show)

newtype TypeCheckM a = TypeCheckM {getTc :: ExceptT TypeCheckError (StateT TypeCheckEnv IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadError TypeCheckError, MonadState TypeCheckEnv, MonadIO)

assertType :: TsType -> TsType -> TypeCheckM ()
assertType expected got =
  if expected /= got
    then throwError $ TypeMismatch expected got
    else pure ()

checkNode :: AST -> TypeCheckM TsType
checkNode (Assign name value) = checkAssign name value
checkNode (ExprStmt (PrimType ty)) = checkPrimType ty
checkNode (ExprStmt expr) = checkExpr expr
checkNode (Function name signature body) = checkFunction name signature body
checkNode (If condition consequence alternative) = checkIf condition consequence alternative
checkNode (Var name value) = checkVar name value
checkNode (While condition body) = checkWhile condition body
checkNode (Return expr) = checkReturn expr

checkPrimType :: PrimitiveType -> TypeCheckM TsType
checkPrimType (Boolean _) = pure BooleanType
checkPrimType (Number _) = pure NumberType
checkPrimType Undefined = pure VoidType
checkPrimType Null = pure VoidType

checkExpr :: Expr -> TypeCheckM TsType
checkExpr (Not booleanExpr) = checkNot booleanExpr
checkExpr (Add left right) = checkAdd left right
checkExpr (Length expr) = checkArrayLength expr
checkExpr (Call callee arguments) = checkCall callee arguments
checkExpr (Array args) = checkArray args
checkExpr (ArrayLookup arg index) = checkArrayLookup arg index

checkNot :: Expr -> TypeCheckM TsType
checkNot node = do
  checkedNode <- checkExpr node
  assertType BooleanType checkedNode
  pure BooleanType

checkAdd :: Expr -> Expr -> TypeCheckM TsType
checkAdd left right = do
  checkedLeft <- checkExpr left
  checkedRight <- checkExpr right
  assertType NumberType checkedLeft
  assertType NumberType checkedRight
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
         in env{functions = newFunctions}
    )

getFunction :: Text -> TypeCheckM Fn
getFunction name = do
  result <- State.gets (\TypeCheckEnv{functions} -> Map.lookup name functions)
  case result of
    Just a -> pure a
    Nothing -> throwError $ UndefinedVariable name

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
