module Homework4.Task2 where

manHeaps :: (Int, Int) -> [(Int, Int)]
manHeaps (a, b) = filter isCorrectHeaps
    [ (a - 1, b    ), (a   *   2, b `div` 2)
    , (a    , b - 1), (a `div` 2, b   *   2)
    ]
  where
    isCorrectHeaps (x, y) = x >= 0 && y >= 0

zeroInMin :: (Int, Int) -> Int
zeroInMin h = zmin 0 [h]
  where zmin n hl = if any (\(a, b) -> a == 0 && b == 0) hl
                    then n
                    else zmin (n + 1) $ hl >>= manHeaps
