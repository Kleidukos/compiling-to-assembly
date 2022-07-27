module Lexer where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec (Parsec, between, (<?>))
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

skipLineComment :: Parser ()
skipLineComment = L.skipLineComment "//"

skipBlockComment :: Parser ()
skipBlockComment = L.skipBlockComment "/*" "*/"

consumeSpaces :: Parser ()
consumeSpaces = L.space space1 skipLineComment skipBlockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme consumeSpaces

symbol :: Text -> Parser Text
symbol = L.symbol consumeSpaces

integer :: Parser Integer
integer = lexeme (L.signed consumeSpaces L.decimal)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semicolon :: Parser Text
semicolon = lexeme (symbol ";") <?> ";"

assign :: Parser Text
assign = lexeme (symbol "=") <?> "="

comma :: Parser Text
comma = lexeme (symbol ",") <?> ","
