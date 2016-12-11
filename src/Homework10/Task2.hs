module Homework10.Task2 where

import           Language.Haskell.TH
import           System.Environment  (getEnv)

thEnv :: Q Exp
thEnv = runIO (getEnv "TH_ENV") >>= pure . LitE . stringL
