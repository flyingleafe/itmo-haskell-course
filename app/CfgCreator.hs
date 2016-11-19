module Main where

import           Homework8          (runConfigurator)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "File name not provided"
    else runConfigurator $ head args
