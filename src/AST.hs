module AST
  ( AST (..)
  , Expr (..)
  , operatorTable
  )
where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Lexer

data Expr
  = Number Integer
  | Boolean Bool
  | Undefined
  | Null
  | Identifier Text
  | Not Expr
  | Equal Expr Expr
  | NotEqual Expr Expr
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Call Text [Expr]
  deriving stock (Eq, Ord, Show)

data AST
  = ExprStmt Expr
  | Return Expr
  | Block [AST]
  | If Expr AST AST
  | Function Text [Text] AST
  | Var Text Expr
  | Assign Text Expr
  | While Expr AST
  deriving stock (Eq, Ord, Show)

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
