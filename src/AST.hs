module AST
  ( AST (..)
  , Expr (..)
  , TsType (..)
  , Fn (..)
  , PrimitiveType (..)
  , operatorTable
  )
where

import Control.Monad.Combinators.Expr
import qualified Data.Foldable as Foldable
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered.Strict as OMap
import Data.Text (Text)
import Data.Text.Display
import qualified Data.Text.Internal.Builder as Builder
import Lexer

data PrimitiveType
  = Number Integer
  | Boolean Bool
  | Null
  | Undefined
  deriving stock (Eq, Ord, Show)

data Expr
  = Add Expr Expr
  | Array [Expr]
  | ArrayLookup Expr Expr
  | Call Text [Expr]
  | Divide Expr Expr
  | Equal Expr Expr
  | Identifier Text
  | Length Expr
  | Multiply Expr Expr
  | Not Expr
  | NotEqual Expr Expr
  | PrimType PrimitiveType
  | Subtract Expr Expr
  deriving stock (Eq, Ord, Show)

data AST
  = Assign Text Expr
  | Block [AST]
  | ExprStmt Expr
  | Function
      Text
      -- ^ name
      Fn
      -- ^ signature
      AST
      -- ^ body
  | If Expr AST AST
  | Return Expr
  | Var Text Expr
  | While Expr AST
  deriving stock (Eq, Ord, Show)

data TsType
  = ArrayType TsType
  | BooleanType
  | FunctionType Fn
  | NumberType
  | VoidType
  deriving stock (Eq, Ord, Show)

instance Display TsType where
  displayBuilder BooleanType = "boolean"
  displayBuilder NumberType = "number"
  displayBuilder VoidType = "void"
  displayBuilder (ArrayType t) = "Array<" <> displayBuilder t <> ">"
  displayBuilder (FunctionType fn) = displayBuilder fn

data Fn = Fn
  { parameters :: OMap Text TsType
  , returnType :: TsType
  }
  deriving stock (Ord, Show)

instance Display Fn where
  displayBuilder (Fn parameters returnType) = "(" <> Builder.fromText typeList <> ")"
    where
      argsList = foldMap ((\t -> display t <> " -> ") . snd) (OMap.assocs parameters)
      typeList = argsList <> display returnType

instance Eq Fn where
  Fn params _ == Fn params' _ =
    reverse (Foldable.toList params) == reverse (Foldable.toList params')

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ -- prefix "+" id
      prefix "!" Not
    ]
  ,
    [ binary "*" Multiply
    , binary "/" Divide
    ]
  ,
    [ binary "+" Add
    , binary "-" Subtract
    ]
  ,
    [ binary "==" Equal
    , binary "!=" NotEqual
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
