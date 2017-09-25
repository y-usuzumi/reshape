module Language.Reshape.Lexer where

import           Control.Monad
import           Data.Char
import           Data.Either.Unwrap
import           Data.Functor            (fmap)
import           Data.List
import           Data.String.Interpolate
import           Debug.Trace
import           Language.Reshape.Token
import           Text.Parsec hiding (token)
import           Text.Printf

type Lexer t = forall s u m. Stream s m Char => ParsecT s u m t

-------
-- Prim
-------

positiveInteger :: Lexer Integer
positiveInteger = foldl' (\a i -> a * 10 + fromIntegral (digitToInt i)) 0 <$> many1 digit

negativeInteger :: Lexer Integer
negativeInteger = char '-' *> positiveInteger >>= return . negate

int :: Lexer Integer
int = try $ positiveInteger <|> negativeInteger

-- TODO
float :: Lexer Double
float = undefined

escape :: Lexer String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Lexer Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Lexer String
character = fmap return nonEscape <|> escape

str :: Lexer String
str = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings

ident :: Lexer String
ident = do
  l <- char '_' <|> letter
  r <- many $ char '_' <|> alphaNum
  return (l:r)

inlineSpaces :: Lexer ()
inlineSpaces = skipMany (oneOf " \t")

inlineSpaces1 :: Lexer ()
inlineSpaces1 = skipMany1 (oneOf " \t")

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
tInt = TInt <$> int

tFloat :: Lexer Token
tFloat = TFloat <$> float

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
  TInfixOp <$> many1 (oneOf "!@#$%^&*-+=|./?:<>")
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

tokenizer :: Lexer [Token]
tokenizer = many token

tokenize :: String -> Either ParseError [Token]
tokenize = parse tokenizer ""
