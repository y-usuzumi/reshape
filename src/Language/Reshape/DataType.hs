module Language.Reshape.DataType where

import           Control.Monad.IO.Class
import qualified Reshape.Engine.JSON    as JSON
import           Text.Printf

data ReshapeModelValue = JSONValue JSON.JSONQueryContext

data Value where
  VNull :: Value
  VInt :: Integer -> Value
  VFloat :: Double -> Value
  VString :: String -> Value
  VList :: [Value] -> Value
  VQuery :: String -> Value
  VFunc :: (Value -> IO Value) -> Value
  VReshapeModel :: ReshapeModelValue -> Value

data Type = TInt
          | TFloat
          | TString
          | TList
          | TQuery
          | TFunc
          | TReshapeModel
          | TAny [Type]
          deriving Show

getType :: Value -> Type
getType (VInt _)          = TInt
getType (VFloat _)        = TFloat
getType (VString _)       = TString
getType (VList _)         = TList
getType (VQuery _)        = TQuery
getType (VFunc _)         = TFunc
getType (VReshapeModel _) = TReshapeModel


class ReshapeQueryableModel context where
  load :: MonadIO mio => String -> mio context
  query :: context -> String -> context
  write :: MonadIO mio => Value -> context -> mio ()

instance ReshapeQueryableModel JSON.JSONQueryContext where
  load = return . JSON.load
  query = JSON.query
  write (VString s) c = liftIO $ printf "Writing %s to %s\n" s (JSON.source c)
  write (VInt i) c = liftIO $ printf "Writing %d to %s\n" i (JSON.source c)
