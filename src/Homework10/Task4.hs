module Homework10.Task4
       ( fibNum
       ) where

import           Data.List           ((!!))
import           Language.Haskell.TH

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibNum :: Int -> Q Exp
fibNum n = pure . LitE . integerL $ fibs !! n
