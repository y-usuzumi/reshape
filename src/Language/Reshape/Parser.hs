module Language.Reshape.Parser where

import           Control.Monad
import           Data.Char
import           Data.Either.Unwrap
import           Data.Functor            (fmap)
import           Data.List
import           Data.String.Interpolate
import           Debug.Trace
import           Language.Reshape.AST
import           Language.Reshape.Lexer
import           Language.Reshape.Token
import           Text.Parsec
import           Text.Printf

type Parser t = forall s u m. Stream s m Token => ParsecT s u m t

-------
-- Prim
-------

satisfy :: (Show t, Eq t, Stream s m t) => (t -> Bool) -> ParsecT s u m t
satisfy = tokenPrim (\c -> show [c])
          (\pos c _cs -> updatePosChar pos c)
          (\c -> if f c then Just c else Nothing)

----------
-- Literal
----------

pInt :: Parser Literal
pInt = do
  TInt i <- satisfy isTInt
  return $ EInt i

pFloat :: Parser Literal
pFloat = do
  TFloat f <- satisfy isTFloat
  return $ EFloat f

pString :: Parser Literal
pString = do
  TString s <- satisfy isTString
  return $ EString s

pSource :: Parser Literal
pSource = do
  (TSource f) <- tSource
  return $ LSource f

pQuery :: Parser Literal
pQuery = do
  (TQuery f) <- tQuery
  return $ LQuery f

pInfixOp :: Parser Literal
pInfixOp = do
  (TInfixOp f) <- tInfixop
  return $ LInfixOp f

pList :: Parser Literal
pList = do
  void $ tLSB
  exprs <- pExpr `sepBy` tComma
  void $ tRSB
  return $ LList exprs

-------
-- Expr
-------

pELiteral :: Parser Expr
pELiteral = ELiteral <$> pLiteral

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
  expr <- rvExpr
  return $ SLet binding expr

rvWrite :: Parser Stmt
rvWrite = do
  writer <- rvExpr
  inlineSpaces
  string "|>"
  inlineSpaces
  writee <- rvExpr
  return $ SWrite writer writee

rvExprStmt :: Parser Stmt
rvExprStmt = SExprStmt <$> rvExpr

rvStmt :: Parser Stmt
rvStmt = rvBind
  <|> try rvWrite
  <|> rvExprStmt


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
