module Language.Reshape.Interpreter where

import           Control.Arrow
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.List
import qualified Data.Map                  as M
import           Debug.Trace
import           Language.Reshape.AST      as AST
import           Language.Reshape.DataType
import qualified Reshape.Engine.JSON

data InterpreterError = NoError
                      | UndefinedVariable String
                      | UnsupportedEngineType String
                      | TypeError Type Type
                      | NotCallable Type
                      deriving Show
instance Exception InterpreterError

-- TODO: Move to other place
-- showType :: Type -> String
-- showType TInt = "integer"
-- showType TFloat = "float"
-- showType TString = "string"
-- showType TList = "list"
-- showType TQuery = "query"
-- showType TFunc = "function"
-- showType TReshapeModel = "model"


type Scope = M.Map String Value

loadValue :: String -> String -> IO Value
loadValue "json" sourcePath = (VReshapeModel . JSONValue) <$> load sourcePath
loadValue sourceType _      = throwM $ UnsupportedEngineType sourceType

writeValue :: Value -> Value -> IO ()
writeValue s (VReshapeModel model) = case model of
  JSONValue context -> write s context
writeValue _ v = throwM $ TypeError TReshapeModel (getType v)

evalLiteral :: Scope -> AST.Literal -> IO Value
evalLiteral _ (LInt int)     = return $ VInt int
evalLiteral _ (LFloat float) = return $ VFloat float
evalLiteral _ (LString s)    = return $ VString s
evalLiteral scope (LList l)  = VList <$> (sequenceA $ fmap (eval scope) l)
evalLiteral _ (LSource sourceType sourcePath) = loadValue sourceType sourcePath
evalLiteral _ (LQuery q) = return $ VQuery q
evalLiteral scope (LInfixOp op) = case op of
  "?" -> return $ VFunc execQuery

evalVar :: Scope -> String -> IO Value
evalVar scope s = case s `M.lookup` scope of
  Nothing -> throwM $ UndefinedVariable s
  Just v  -> return $ v

evalApp :: Scope -> AST.Expr -> AST.Expr -> IO Value
evalApp scope l r = do
  lvalue <- eval scope l
  rvalue <- eval scope r
  case lvalue of
    VFunc f -> f rvalue
    v       -> throwM $ NotCallable (getType v)

eval :: Scope -> AST.Expr -> IO Value
eval scope (ELiteral literal) = evalLiteral scope literal
eval scope (EVar var)         = evalVar scope var
eval scope (EApp l r)         = evalApp scope l r

exec :: AST.Stmt -> StateT Scope IO ()
exec (SLet s expr) = do
  return $ trace s ()
  scope <- get
  value <- lift $ eval scope expr
  bindValue s value
  where
    bindValue :: String -> Value -> StateT Scope IO ()
    bindValue s value = modify (M.insert s value)
exec (SWrite l r) = do
  scope <- get
  writer <- lift $ eval scope l
  writee <- lift $ eval scope r
  lift $ writeValue writer writee
exec (SApp expr) = do
  scope <- get
  lift $ void $ eval scope expr

execStmts :: [AST.Stmt] -> StateT Scope IO ()
execStmts stmts = mapM_ exec stmts

run :: Program -> IO ()
run (Program stmts) = void $ runStateT (execStmts stmts) initialScope


---------------------
-- Built-in functions
---------------------

functions :: [(String, Value -> IO Value)]
functions = [ ( "echo", echoFunc)
            ]
  where
    echoFunc (VInt int) = print int >> return VNull

initialScope :: M.Map String Value
initialScope = M.fromList (map (second VFunc) functions)

execQuery :: Value -> IO Value
execQuery (VReshapeModel v) = return $ VFunc (execQuery2 v)
  where
    execQuery2 (JSONValue context) (VQuery s) = return $ (VReshapeModel $ JSONValue $ query context s)
execQuery v
  | t <- getType v = throwM $ TypeError t TReshapeModel
