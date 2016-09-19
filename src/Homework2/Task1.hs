module Homework2.Task1 where

safeTail, safeInit :: [a] -> Either String [a]
safeTail [] = Left "List is empty, no head"
safeTail (_:xs) = Right xs
safeInit [] = Left "List is empty, no tail"
safeInit [_] = Right []
safeInit (x:xs) = (x:) <$> safeInit xs

strip :: [a] -> Either String [a]
strip ls = safeTail ls >>= safeInit
