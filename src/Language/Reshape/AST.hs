module Language.Reshape.AST where

data Query = Query String
           deriving (Eq, Show)

data Literal = VInt Integer
           | VFloat Double
           | VString String
           | VList [Expr]
           | VSource String String
           | VQuery Query
           | VInfixOp String
           deriving (Eq, Show)


data Expr = ELiteral Literal
          | EVar String
          | EApp Expr Expr
          deriving (Eq, Show)


data Stmt = SLet {- binding -} String {- value -} Expr
          | SWrite Expr Expr
          | SApp Expr  -- SApp (EApp func value)
          deriving (Eq, Show)

data Program = Program [Stmt]
             deriving (Eq, Show)
