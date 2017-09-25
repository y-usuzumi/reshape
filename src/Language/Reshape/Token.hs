module Language.Reshape.Token where

data Token = TInt Integer
           | TFloat Double
           | TString String
           | TLP
           | TRP
           | TLSB
           | TRSB
           | TComma
           | TIdent String
           | TSource String String
           | TQuery String
           | TKeyword String
           | TInfixOp String
           deriving (Eq, Show)

isTInt :: Token -> Bool
isTInt (TInt _) = True
isTInt _ = False

isTFloat :: Token -> Bool
isTFloat (TFloat _) = True
isTFloat _ = False

isTString :: Token -> Bool
isTString (TString _) = True
isTString _ = False

isTLP :: Token -> Bool
isTLP TLP  = True
isTLP _ = False

isTRP :: Token -> Bool
isTRP TRP  = True
isTRP _ = False

isTLSB :: Token -> Bool
isTLSB TLSB  = True
isTLSB _ = False

isTRSB :: Token -> Bool
isTRSB TRSB  = True
isTRSB _ = False

isTSource :: Token -> Bool
isTSource (TSource _ _) = True
isTSource _ = False

isTIdent :: Token -> Bool
isTIdent (TIdent _) = True
isTIdent _ = False

isTQuery :: Token -> Bool
isTQuery (TQuery _) = True
isTQuery _ = False

isTKeyword :: Token -> Bool
isTKeyword (TKeyword _) = True
isTKeyword _ = False

isTInfixOp :: Token -> Bool
isTInfixOp (TInfixOp _) = True
isTInfixOp _ = False
