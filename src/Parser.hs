{-# OPTIONS_GHC -Wno-unused-imports #-}
module Parser where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Text                      as T
import qualified Text.Megaparsec.Char.Lexer as L

import Lexer
import AST

parseLine :: Text -> Either String AST
parseLine input =
  case runParser (statements <* eof) "<line>" input of
      Left eb -> Left $ errorBundlePretty eb
      Right ast -> Right ast

statements :: Parser AST
statements = do
  consumeSpaces
  stmts <- many parseStatement
  eof
  pure (Block stmts)

-- nonAlphaNumTokens :: [Token Text]
-- nonAlphaNumTokens = "!?¡¿$€%&|*×÷+-/:<=>@^_~"

-- parseText :: Parser AST
-- parseText = do
--   char '"'
--   x <- manyTill L.charLiteral (char '"')
--   pure $ String $ T.pack x

keyword :: Text -> Parser ()
keyword w = string w *> notFollowedBy alphaNumChar *> consumeSpaces

rws :: [Text] -- list of reserved words
rws = ["if","then","else","while","true","false"]

parseTextIdentifier :: Parser Text
parseTextIdentifier =
  (lexeme . try) (identifierParser >>= check)
  where
    check  :: Text -> Parser Text
    check x = if x `elem` rws
              then fail $ "Keyword " <> show x <> " cannot be an identifier"
              else pure x

    identifierParser :: Parser Text
    identifierParser = do
      beginning <- letterChar
      rest <- many (letterChar <|> digitChar)
      pure $ T.pack $ [beginning] <> rest

parseIdentifier :: Parser AST
parseIdentifier = Identifier <$> parseTextIdentifier

parseNumber :: Parser AST
parseNumber = Number <$> integer

parseNegNumber :: Parser AST
parseNegNumber = do
  char '-'
  d <- integer
  pure . Number . negate $ d

parseCall :: Parser AST
parseCall = label "call" $ do
  callee <- parseTextIdentifier
  args <- parens (parseIdentifier `sepBy` comma)
  if callee == "assert"
  then pure $ Assert (head args)
  else pure $ Call callee args

parseAtom :: Parser AST
parseAtom =
  try parseCall
  <|> parseNumber
  <|> parens parseIdentifier

parseUnary :: Parser AST
parseUnary = do
  mSign <- optional (char '-')
  term <- parseAtom
  case mSign of
    Nothing -> pure term
    Just _ -> pure $ Not term

parseStatement :: Parser AST
parseStatement =
      (parseReturn             <?> "Return")
  <|> (parseFunctionStatement  <?> "Function")
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
  pure term

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
parseAssignStatement = do
  name <- parseTextIdentifier
  assign
  value <- parseExpression
  semicolon
  pure $ Assign name value

parseBlockStatement :: Parser AST
parseBlockStatement = do
  stmts <- braces $ many parseStatement
  pure $ Block stmts

parseFunctionStatement :: Parser AST
parseFunctionStatement = do
  keyword "function"
  name <- parseTextIdentifier
  parameters <- parens (parseTextIdentifier `sepBy` comma)
  block@(Block stmts) <- parseBlockStatement
  if name == "main"
  then pure $ Main stmts
  else pure $ Function name parameters block

parseTerm :: Parser AST
parseTerm = label "term" $
      parens parseExpression
  <|> parseNumber
  <|> parseCall
  <|> parseIdentifier

parseExpression :: Parser AST
parseExpression = makeExprParser parseTerm operatorTable <?> "Expression"

operatorTable :: [[Operator Parser AST]]
operatorTable =
  [ [ prefix "+" id
    , prefix "!" Not
    ]
  , [ binary "*" Multiply
    , binary "/" Divide
    ]
  , [ binary "+" Add
    , binary "-" Subtract
    ]
  , [ binary "==" Equal
    , binary "!=" NotEqual
    ]
  ]

binary :: Text -> (AST -> AST -> AST) -> Operator Parser AST
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (AST -> AST) -> Operator Parser AST
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)
