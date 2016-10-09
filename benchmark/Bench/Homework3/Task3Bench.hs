module Bench.Homework3.Task3Bench ( main ) where

import qualified Homework2.Task3   as T
import qualified Homework3.Task2   ()
import qualified Homework3.Task3   as S
import           Homework3.Task4v1 ()

import           Criterion.Main
import           Criterion.Types   (Config (..))
import           Test.QuickCheck   (arbitrary, generate, vectorOf)


arbitraryIntVectorOf :: Int -> IO [Int]
arbitraryIntVectorOf n = generate (vectorOf n arbitrary)

fromListBench :: Int -> Benchmark
fromListBench n = env (arbitraryIntVectorOf n) $
                  \ls -> bench ("fromList: n = " ++ show n) $ whnf (S.fromList :: [Int] -> T.Tree Int) ls

fromBambooListBench :: Int -> Benchmark
fromBambooListBench n = env (return [1..n]) $
                        \ls -> bench ("fromBambooList: n = " ++ show n) $ whnf (S.fromList :: [Int] -> T.Tree Int) ls

main :: IO ()
main = defaultMainWith (defaultConfig { reportFile = Just "avl-tree.html" })
       [ fromListBench 1000
       , fromListBench 100000
       , fromBambooListBench 1000
       , fromBambooListBench 100000
       ]
