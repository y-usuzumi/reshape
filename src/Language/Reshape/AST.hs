module Language.Reshape.AST where

data Literal = LInt Integer
           | LFloat Double
           | LString String
           | LList [Expr]
           | LSource String String
           | LQuery String
           | LInfixOp String
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
