module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> generate "." "_site"
    [src] -> generate src "_site"
    [src, dest] -> generate src dest
    excessArgs -> tooManyArguments excessArgs

tooManyArguments :: [String] -> IO ()
tooManyArguments excessArgs =
  error $ "Too many arguments " <> show excessArgs
