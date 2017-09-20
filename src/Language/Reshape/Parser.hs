module Language.Reshape.Parser where

import           Data.Char
import           Data.Either.Unwrap
import           Data.Functor            (fmap)
import           Data.List
import           Data.String.Interpolate
import           Language.Reshape.AST
import           Text.Parsec

type Parser t = forall s u m. Stream s m Char => ParsecT s u m t

-------
-- Prim
-------

positiveInteger :: Parser Integer
positiveInteger = foldl' (\a i -> a * 10 + fromIntegral (digitToInt i)) 0 <$> many1 digit

negativeInteger :: Parser Integer
negativeInteger = char '-' *> positiveInteger >>= return . negate

int :: Parser Integer
int = positiveInteger <|> negativeInteger

-- TODO
float :: Parser Double
float = undefined

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

str :: Parser String
str = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings

ident :: Parser String
ident = do
  l <- char '_' <|> letter
  r <- many1 $ char '_' <|> alphaNum
  return (l:r)

----------
-- Literal
----------

rvInteger :: Parser Literal
rvInteger = VInt <$> int

-- TODO
rvFloat :: Parser Literal
rvFloat = VFloat <$> float

rvString :: Parser Literal
rvString = VString <$> str

rvList :: Parser Literal
rvList = VList <$> between (char '[') (char ']') (rvExpr `sepBy` (char ','))

rvSource :: Parser Literal
rvSource = do
  string "<#"
  identifier <- ident
  path <- manyTill anyChar (try (char '>'))
  char '>'
  return $ VSource identifier path

rvQuery :: Parser Literal
rvQuery = do
  string "</"
  -- FIXME: should be better than ident
  q <- manyTill anyChar (try (char '>'))
  char '>'
  return $ VQuery $ Query q

rvInfixOp :: Parser Literal
rvInfixOp = VInfixOp <$> many1 (oneOf "!@#$%^&*-+=|./?")

rvLiteral :: Parser Literal
rvLiteral = rvInteger
  <|> rvFloat
  <|> rvString
  <|> rvList
  <|> rvSource
  <|> rvQuery

-------
-- Expr
-------

rvELiteral :: Parser Expr
rvELiteral = ELiteral <$> rvLiteral

rvVar :: Parser Expr
rvVar = EVar <$> ident

rvAppCall :: Parser Expr
rvAppCall = do
  l <- rvExpr
  many1 $ oneOf " \t"
  r <- rvExpr
  return $ EApp l r

rvAppInfix :: Parser Expr
rvAppInfix = do
  l <- rvExpr
  spaces
  i <- rvInfixOp
  spaces
  r <- rvExpr
  return $ EApp (ELiteral i) l `EApp` r

rvEnclosed :: Parser Expr
rvEnclosed = do
  char '('
  spaces
  expr <- rvExpr
  spaces
  char ')'
  return expr

rvExpr :: Parser Expr
rvExpr = rvEnclosed
  <|> rvAppInfix
  <|> rvAppCall
  <|> rvVar
  <|> rvELiteral

-------
-- Stmt
-------

rvBind :: Parser Stmt
rvBind = do
  string "let"
  many1 space
  binding <- ident
  spaces
  char '='
  spaces
  expr <- rvExpr
  return $ SLet binding expr

rvAppStmt :: Parser Stmt
rvAppStmt = SApp <$> rvAppCall

rvStmt :: Parser Stmt
rvStmt = rvBind
  <|> rvAppStmt


----------
-- Program
----------

rvProgram :: Parser Program
rvProgram = do
  many endOfLine
  stmts <- (rvStmt <* spaces) `sepBy` (many1 eof)
  many endOfLine
  return $ Program stmts

prog :: String
prog = [i||]

p :: Program
p = fromRight $ parse rvProgram "" prog
