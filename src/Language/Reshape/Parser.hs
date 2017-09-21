module Language.Reshape.Parser where

import           Control.Monad
import           Data.Char
import           Data.Either.Unwrap
import           Data.Functor            (fmap)
import           Data.List
import           Data.String.Interpolate
import           Debug.Trace
import           Language.Reshape.AST
import           Text.Parsec
import           Text.Printf

type Parser t = forall s u m. Stream s m Char => ParsecT s u m t

-------
-- Prim
-------

positiveInteger :: Parser Integer
positiveInteger = foldl' (\a i -> a * 10 + fromIntegral (digitToInt i)) 0 <$> many1 digit

negativeInteger :: Parser Integer
negativeInteger = char '-' *> positiveInteger >>= return . negate

int :: Parser Integer
int = try $ positiveInteger <|> negativeInteger

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
  r <- many $ char '_' <|> alphaNum
  return (l:r)

inlineSpaces :: Parser ()
inlineSpaces = skipMany (oneOf " \t")

inlineSpaces1 :: Parser ()
inlineSpaces1 = skipMany1 (oneOf " \t")

----------
-- Literal
----------

rvInteger :: Parser Literal
rvInteger = LInt <$> int

-- TODO
rvFloat :: Parser Literal
rvFloat = LFloat <$> float

rvString :: Parser Literal
rvString = LString <$> str

rvList :: Parser Literal
rvList = LList <$> between (char '[') (char ']') (rvExpr `sepBy` (char ','))

rvSource :: Parser Literal
rvSource = do
  try $ string "<#"
  inlineSpaces
  identifier <- ident
  char ':'
  inlineSpaces
  path <- manyTill anyChar (try (inlineSpaces *> (string "#>")))
  return $ LSource identifier path

rvQuery :: Parser Literal
rvQuery = do
  try $ string "<?"
  inlineSpaces
  q <- manyTill anyChar (try (inlineSpaces *> (string "?>")))
  return $ LQuery q

rvInfixOp :: Parser Literal
rvInfixOp = let  in do
  notFollowedBy $ excludedOp "="
  notFollowedBy $ excludedOp "<#"
  notFollowedBy $ excludedOp "#>"
  notFollowedBy $ excludedOp "<?"
  notFollowedBy $ excludedOp "?>"
  notFollowedBy $ excludedOp "|>"
  LInfixOp <$> many1 (oneOf "!@#$%^&*-+=|./?:<>")
  where
    opChars = "!@#$%^&*-+=|./?:<>"
    excludedOp s = string s >> notFollowedBy (oneOf opChars)

rvLiteral :: Parser Literal
rvLiteral = (rvInteger
  -- <|> rvFloat
  <|> rvString
  <|> rvList
  <|> rvInfixOp
  <|> rvSource
  <|> rvQuery)

-------
-- Expr
-------

rvELiteral :: Parser Expr
rvELiteral = ELiteral <$> rvLiteral

rvVar :: Parser Expr
rvVar = EVar <$> ident

rvAppCall :: Parser Expr
rvAppCall = do
  l <- rvAtomExpr
  ls <- many1 (try (inlineSpaces1 *> rvAtomExpr))
  (foldl1 EApp) <$> (foldApp (l:ls))
  where
    foldApp :: [Expr] -> Parser [Expr]
    foldApp [] = return []
    foldApp (a:[]) = return [a]
    foldApp (op@(ELiteral (LInfixOp _)):a:s) = foldApp $ (EApp op a):s
    foldApp (a:op@(ELiteral (LInfixOp opS)):[]) = unexpected (printf "Op %s is missing operand(s)" opS)
    foldApp (a:op@(ELiteral (LInfixOp _)):c:s) = foldApp $ (EApp (EApp op a) c):s
    foldApp (a:b:s) = foldApp $ (EApp a b):s

rvEnclosed :: Parser Expr
rvEnclosed = do
  try $ char '('
  spaces
  expr <- rvExpr
  spaces
  char ')'
  return expr

rvAtomExpr :: Parser Expr
rvAtomExpr = rvEnclosed
  <|> rvELiteral
  <|> rvVar

rvExpr :: Parser Expr
rvExpr = try rvAppCall
  <|> rvAtomExpr

-------
-- Stmt
-------

rvBind :: Parser Stmt
rvBind = do
  try $ string "let"
  inlineSpaces1
  binding <- ident
  inlineSpaces
  char '='
  inlineSpaces
  expr <- rvELiteral
  return $ SLet binding expr

rvWrite :: Parser Stmt
rvWrite = do
  writer <- rvExpr
  inlineSpaces
  string "|>"
  inlineSpaces
  writee <- rvExpr
  return $ SWrite writer writee

-- rvAppStmt :: Parser Stmt
-- rvAppStmt = SApp <$> rvAppCall

rvStmt :: Parser Stmt
rvStmt = rvBind
  <|> try rvWrite


----------
-- Program
----------

rvProgram :: Parser Program
rvProgram = do
  many (inlineSpaces >> endOfLine)
  stmts <- rvStmt `endBy` (many1 $ inlineSpaces >> endOfLine)
  spaces
  eof
  return $ Program stmts

parseProgram :: String -> Either ParseError Program
parseProgram prog = parse rvProgram "" prog
