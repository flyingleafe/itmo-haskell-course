module Main where

import           System.Environment (getArgs)

import           Homework10         (getDirectory, runWalker)

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    []          -> putStrLn "Provide path to directory"
    (dirname:_) -> getDirectory dirname >>= runWalker
