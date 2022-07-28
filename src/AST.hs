module AST
  ( AST (..)
  , Expr (..)
  , TsType (..)
  , Fn (..)
  , operatorTable
  )
where

import Control.Monad.Combinators.Expr
import qualified Data.Foldable as Foldable
import Data.Map.Ordered (OMap)
import Data.Text (Text)
import Lexer

data Expr
  = Add Expr Expr
  | Array [Expr]
  | ArrayLookup Expr Expr
  | Boolean Bool
  | Call Text [Expr]
  | Divide Expr Expr
  | Equal Expr Expr
  | Identifier Text
  | Length Expr
  | Multiply Expr Expr
  | Not Expr
  | NotEqual Expr Expr
  | Null
  | Number Integer
  | Subtract Expr Expr
  | Undefined
  deriving stock (Eq, Ord, Show)

data AST
  = ExprStmt Expr
  | Return Expr
  | Block [AST]
  | If Expr AST AST
  | Function Text Fn AST
  | Var Text Expr
  | Assign Text Expr
  | While Expr AST
  deriving stock (Eq, Ord, Show)

data TsType
  = BooleanType
  | NumberType
  | VoidType
  | ArrayType TsType
  | FunctionType Fn
  deriving stock (Eq, Ord, Show)

data Fn = Fn
  { parameters :: OMap Text TsType
  , returnType :: TsType
  }
  deriving stock (Ord, Show)

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
