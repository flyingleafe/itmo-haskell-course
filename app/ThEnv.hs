{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Homework10 (fibNum, showPretty, thEnv)

myEnv :: String
myEnv = $(thEnv)

data Foo = Foo
  { foo :: String
  , bar :: Int
  }

showPretty ''Foo

main :: IO ()
main = do
  putStrLn myEnv
  print $ Foo "asdfsadf" 2131
  print ($(fibNum 10) :: Int)
