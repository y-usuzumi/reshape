module Language.Reshape.AST where

data Expr = EInt Integer
          | EFloat Double
          | EString String
          | ESource String String
          | EQuery String
          | EInfixOp String
          | EList [Expr]
          | EVar String
          | EApp Expr Expr
          deriving (Eq, Show)

data Stmt = SLet {- binding -} String {- value -} Expr
          | SWrite Expr Expr
          | SExprStmt Expr  -- SExpr
          deriving (Eq, Show)

data Program = Program [Stmt]
             deriving (Eq, Show)
