module Homework10.Task1 where

import           Control.Lens        (element, (.~))
import           Data.Function       ((&))
import           Language.Haskell.TH

selN :: Int -> Int -> Q Exp
selN n k = do
  x <- newName "x"
  let tup = replicate n WildP & element (k - 1) .~ VarP x
  return $ LamE [TupP tup] (VarE x)
