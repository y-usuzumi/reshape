module Language.Reshape.Runner where

import Language.Reshape.Parser
import Language.Reshape.Interpreter


runProgramFile :: FilePath -> IO ()
runProgramFile fileName = do
  content <- readFile fileName
  case parseProgram content of
    Left e -> print e
    Right prog -> run prog
