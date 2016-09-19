module Homework1.Task2 where

-- This implementation assumes that the first list in row is shorter or equal than others
-- More consistent and meaningful behaviors (result list length is length of shortest/longest list in input)
-- are impossible together with given laziness requirement - in order to detect the end of some list or find some
-- list which is not ended yet we have to check possibly infinite number of lists
zipN :: ([a] -> b) -> [[a]] -> [b]
zipN _ [] = []
zipN _ ([]:_) = []
zipN f lists = f (map head lists) : zipN f (map tail lists)
