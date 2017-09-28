module Language.Reshape.Lexer where

import           Control.Monad
import           Data.Char
import           Data.Either.Unwrap
import           Data.Functor            (fmap)
import           Data.List
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Void
import           Debug.Trace
import           Language.Reshape.Token
import           Text.Megaparsec         hiding (Token, token, space)
import qualified          Text.Megaparsec.Lexer   as L
import           Text.Printf

type Lexer = Parsec Void String

-------
-- Prim
-------

str :: Lexer String
str = catMaybes <$> (char '"' >> manyTill ch (char '"'))
  where ch = (Just <$> L.charLiteral) <|> (Nothing <$ string "\\&")

ident :: Lexer String
ident = do
  l <- char '_' <|> letterChar
  r <- many $ char '_' <|> alphaNumChar
  return (l:r)

inlineSpaceChar :: Lexer Char
inlineSpaceChar = oneOf " \t"

lineComment :: Lexer ()
lineComment = L.skipLineComment "//"

blockComment :: Lexer ()
blockComment = L.skipBlockComment "/*" "*/"

space :: Lexer ()
space = L.space (void spaceChar) lineComment blockComment

iSpace :: Lexer ()
iSpace = L.space (void inlineSpaceChar) lineComment blockComment

symbol :: String -> Lexer String
symbol = L.symbol space

---------
-- Tokens
---------

tLP :: Lexer Token
tLP = char '(' >> pure TLP

tRP :: Lexer Token
tRP = char ')' >> pure TRP

tLSB :: Lexer Token
tLSB = char '[' >> pure TLSB

tRSB :: Lexer Token
tRSB = char ']' >> pure TRSB

tComma :: Lexer Token
tComma = char ',' >> pure TComma

tInt :: Lexer Token
tInt = TInt <$> L.integer

tFloat :: Lexer Token
tFloat = TFloat <$> L.float

tString :: Lexer Token
tString = TString <$> str

tIdent :: Lexer Token
tIdent = TIdent <$> ident

tSource :: Lexer Token
tSource = do
  try $ string "<#"
  inlineSpaces
  identifier <- ident
  char ':'
  inlineSpaces
  path <- manyTill anyChar (try (inlineSpaces *> (string "#>")))
  return $ TSource identifier path

tQuery :: Lexer Token
tQuery = do
  try $ string "<?"
  inlineSpaces
  q <- manyTill anyChar (try (inlineSpaces *> (string "?>")))
  return $ TQuery q

tKeyword :: Lexer Token
tKeyword = TKeyword <$> string "let"

tInfixOp :: Lexer Token
tInfixOp = do
  notFollowedBy $ excludedOp "="
  notFollowedBy $ excludedOp "<#"
  notFollowedBy $ excludedOp "#>"
  notFollowedBy $ excludedOp "<?"
  notFollowedBy $ excludedOp "?>"
  notFollowedBy $ excludedOp "|>"
  TInfixOp <$> some (oneOf "!@#$%^&*-+=|./?:<>")
  where
    opChars = "!@#$%^&*-+=|./?:<>"
    excludedOp s = string s >> notFollowedBy (oneOf opChars)

token :: Lexer Token
token = ( tInt
        -- <|> tFloat
          <|> tString
          <|> tKeyword
          <|> tIdent
          <|> tLP
          <|> tRP
          <|> tLSB
          <|> tRSB
          <|> tComma
          <|> tSource
          <|> tQuery
          <|> tInfixOp
        )
