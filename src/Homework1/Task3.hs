module Homework1.Task3 ( mergeSort, randomIntList ) where

import           System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:xs) = (x:a, b)
  where (b, a) = split xs

merge :: [Int] -> [Int] -> [Int]
merge [] ls = ls
merge ls [] = ls
merge as@(a:as') bs@(b:bs') = if a < b
                              then a : merge as' bs
                              else b : merge as bs'

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort xs') (mergeSort xs'')
  where (xs', xs'') = split xs
