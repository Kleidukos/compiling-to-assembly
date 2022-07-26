module AST 
  ( AST(..)
  ) where

import Data.Text (Text)

data AST
  = Number Integer
  | Identifier Text
  | Not AST
  | Equal AST AST
  | NotEqual AST AST
  | Add AST AST
  | Subtract AST AST
  | Multiply AST AST
  | Divide AST AST
  | Call Text [AST]
  | Return AST
  | Block [AST]
  | If AST AST AST
  | Function Text [Text] AST
  | Var Text AST
  | Assign Text AST 
  | While AST AST
  | Main [AST]
  | Assert AST
  deriving stock (Eq, Ord, Show)
