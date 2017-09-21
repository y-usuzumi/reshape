module Main where

import Language.Reshape.Runner
import System.Environment

main :: IO ()
main = do
  files <- getArgs
  mapM_ runProgramFile files
