module Language.Reshape.Interpreter where

import Data.Map
import qualified Language.Reshape.AST as AST
import qualified Language.Reshape.DataType as DT

type Scope = Map String DT.Value
