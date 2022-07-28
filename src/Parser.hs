{-# OPTIONS_GHC -Wno-unused-imports #-}

module Parser where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import qualified Data.Map.Ordered.Strict as OMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import Lexer

parseLine :: Text -> Either String AST
parseLine input =
  case runParser (statements <* eof) "<line>" input of
    Left eb -> Left $ errorBundlePretty eb
    Right ast -> Right ast

parse :: forall a. (Show a) => Parser a -> Text -> IO ()
parse parser input =
  case runParser parser "<line>" input of
    Left eb -> putStrLn $ errorBundlePretty eb
    Right result -> print result

-- nonAlphaNumTokens :: [Token Text]
-- nonAlphaNumTokens = "!?¡¿$€%&|*×÷+-/:<=>@^_~"

-- parseText :: Parser AST
-- parseText = do
--   char '"'
--   x <- manyTill L.charLiteral (char '"')
--   pure $ String $ T.pack x

keyword :: Text -> Parser ()
keyword w = string w *> notFollowedBy alphaNumChar *> consumeSpaces

tsTypeString :: Text -> Parser ()
tsTypeString w = string w *> notFollowedBy alphaNumChar *> consumeSpaces

rws :: [Text] -- list of reserved words
rws = ["if", "then", "else", "while", "true", "false"]

-----------------------
-- ┌───────────────┐ --
-- │  Expressions  │ --
-- └───────────────┘ --
-----------------------

-- ID = token(/[a-zA-Z_][a-zA-Z0-9_]*/y);
parseTextIdentifier :: Parser Text
parseTextIdentifier =
  (lexeme . try) (identifierParser >>= check)
  where
    check :: Text -> Parser Text
    check x =
      if x `elem` rws
        then fail $ "Keyword " <> show x <> " cannot be an identifier"
        else pure x

    identifierParser :: Parser Text
    identifierParser = do
      beginning <- letterChar <|> char '_'
      rest <- many (letterChar <|> digitChar <|> char '_')
      pure $ T.pack $ [beginning] <> rest

parseIdentifier :: Parser Expr
parseIdentifier = Identifier <$> parseTextIdentifier

parseNumber :: Parser Expr
parseNumber = Number <$> integer

parseBoolean :: Parser Expr
parseBoolean = label "boolean" $ do
  boolean <- (keyword "true" $> True) <|> (keyword "false" $> False)
  pure $ Boolean boolean

parseCursed :: Parser Expr
parseCursed =
  label "undefined or null" $
    do
      keyword "undefined" $> Undefined
      <|> keyword "null" $> Null

parseArray :: Parser Expr
parseArray =
  label "array" $
    Array <$> brackets (parseIdentifier `sepBy` comma)

parseArrayType :: Parser TsType
parseArrayType = label "array type" $ do
  tsTypeString "Array" *> notFollowedBy alphaNumChar
  tsType <- chevrons parseTsType
  pure $ ArrayType tsType

parseTsType :: Parser TsType
parseTsType =
  label "typescript type" $
    tsTypeString "void" $> VoidType
      <|> tsTypeString "boolean" $> BooleanType
      <|> tsTypeString "number" $> NumberType
      <|> parseArrayType

parseArrayLookup :: Parser Expr
parseArrayLookup =
  label "array lookup" $
    ArrayLookup
      <$> parseIdentifier
      <*> brackets parseExpression

-- args <- (expression (COMMA expression)*)?
-- call <- ID LEFT_PAREN args RIGHT_PAREN
parseCall :: Parser Expr
parseCall = label "call" $ do
  callee <- parseTextIdentifier
  args <- parens (parseExpression `sepBy` comma)
  pure $ Call callee args

parseTerm :: Parser Expr
parseTerm =
  label "term" $
    parseNumber
      <|> parseBoolean
      <|> parseCursed
      <|> parens parseExpression
      <|> try parseArrayLookup
      <|> parseArray
      <|> try parseCall
      <|> parseIdentifier

parseExpression :: Parser Expr
parseExpression = makeExprParser parseTerm operatorTable <?> "Expression"

----------------------
-- ┌──────────────┐ --
-- │  Statements  │ --
-- └──────────────┘ --
----------------------

statements :: Parser AST
statements = do
  consumeSpaces
  stmts <- many parseStatement
  eof
  pure (Block stmts)

parseStatement :: Parser AST
parseStatement =
  (parseReturn <?> "Return")
    <|> (parseFunctionStatement <?> "Function")
    <|> (parseIfStatement <?> "If")
    <|> (parseWhileStatement <?> "While")
    <|> (parseVarStatement <?> "Var")
    <|> try (parseAssignStatement <?> "Assign")
    <|> (parseBlockStatement <?> "Block")
    <|> (parseExpressionStatement <?> "Expression")

parseReturn :: Parser AST
parseReturn = do
  keyword "return"
  term <- parseExpression
  semicolon
  pure $ Return term

parseExpressionStatement :: Parser AST
parseExpressionStatement = do
  term <- parseExpression
  semicolon
  pure (ExprStmt term)

parseIfStatement :: Parser AST
parseIfStatement = do
  keyword "if"
  conditional <- parens parseExpression
  consequence <- parseStatement
  keyword "else"
  alternative <- parseStatement
  pure $ If conditional consequence alternative

parseWhileStatement :: Parser AST
parseWhileStatement = do
  keyword "while"
  conditional <- parens parseExpression
  body <- parseStatement
  pure $ While conditional body

parseVarStatement :: Parser AST
parseVarStatement = do
  keyword "var"
  name <- parseTextIdentifier
  assign
  value <- parseExpression
  semicolon
  pure $ Var name value

parseAssignStatement :: Parser AST
parseAssignStatement = label "assign" $ do
  name <- parseTextIdentifier
  assign
  value <- parseExpression
  semicolon
  pure $ Assign name value

parseBlockStatement :: Parser AST
parseBlockStatement = label "block" $ do
  stmts <- braces $ many parseStatement
  pure $ Block stmts

-- | Defaults to 'NumberType' if no annotation is found.
parseOptionalTypeAnnotation :: Parser TsType
parseOptionalTypeAnnotation = label "optional type annotation" $ do
  mType <- optional (colon *> parseTsType)
  case mType of
    Nothing -> pure NumberType
    Just t -> pure t

parseParameter :: Parser (Text, TsType)
parseParameter = label "parameter" $ do
  param <- parseTextIdentifier
  t <- parseOptionalTypeAnnotation
  pure (param, t)

parseFunctionStatement :: Parser AST
parseFunctionStatement = label "function" $ do
  keyword "function"
  name <- parseTextIdentifier
  parsedParams <- parens (parseParameter `sepBy` comma)
  returnType <- parseOptionalTypeAnnotation
  body <- parseBlockStatement
  let parameters = Fn (OMap.fromList parsedParams) returnType
  pure $ Function name parameters body
