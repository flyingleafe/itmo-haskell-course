{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Homework9.Task1Bench ( main ) where

import           Control.Monad     (foldM, forM_)
import           Control.Monad.ST
import           Criterion.Main
import           Criterion.Types   (Config (..))
import           Data.Array.MArray (MArray (..), newArray, newArray_, readArray)
import           Data.DList        (DList (..))
import qualified Data.DList        as DL
import           Data.List         (foldl')
import           Data.Sequence     (Seq, (|>))
import qualified Data.Sequence     as Seq
import           Test.QuickCheck   (arbitrary, generate, vectorOf)

import           Homework9

class Pushable t where
  push :: a -> t a -> t a
  pushN :: Int -> t Int -> t Int
  pushN n ls = foldl' (flip push) ls [0..n]

instance Pushable [] where
  push a ls = ls ++ [a]

instance Pushable DList where
  push = flip DL.snoc

instance Pushable Seq where
  push = flip (|>)

pushVecs :: Int -> ()
pushVecs n = runST $ newArray_ (0, 1) >>= forM_ [0..n] . pushBack

pushListsBench :: Int -> Benchmark
pushListsBench n = env (return ([] :: [Int])) $
                   bench ("pushLists: n = " ++ show n) . whnf (pushN n)

pushDLBench :: Int -> Benchmark
pushDLBench n = env (return (DL.fromList [] :: DList Int)) $
                bench ("pushDLists: n = " ++ show n) . whnf (pushN n)

pushSeqBench :: Int -> Benchmark
pushSeqBench n = env (return Seq.empty) $
                 bench ("pushSeqs: n = " ++ show n) . whnf (pushN n)

pushVecBench :: Int -> Benchmark
pushVecBench n = bench ("pushVectors: n = " ++ show n) $ whnf pushVecs n

idxSum :: Int -> (Int, [Int])
idxSum n = let ls = [0..n]
           in foldl' (\(s, l) k -> (s + l !! (n - k), l)) (0, ls) [0..n]

indexListsBench :: Int -> Benchmark
indexListsBench n = bench ("indexLists: n = " ++ show n) $ whnf idxSum n

idxSeqSum :: Int -> (Int, Seq Int)
idxSeqSum n = let sq = Seq.fromList [0..n]
              in foldl' (\(s, sq') k -> (s + Seq.index sq' (n - k), sq')) (0, sq) [0..n]

indexSeqsBench :: Int -> Benchmark
indexSeqsBench n = bench ("indexSeqs: n = " ++ show n) $ whnf idxSeqSum n

-- for type inference
readVec :: STVector s Int a -> Int -> ST s a
readVec = readArray

idxSumVec :: Int -> Int
idxSumVec n = runST $ do
  vec <- newArray (0, n) 5
  foldM (\s i -> (+s) <$> readVec vec (n - i)) 0 [0..n]

indexVectorsBench :: Int -> Benchmark
indexVectorsBench n = bench ("indexVectors: n = " ++ show n) $ whnf idxSumVec n

main :: IO ()
main = let n = 10000
           m = 10000
       in defaultMainWith (defaultConfig { reportFile = Just "arrays-benches.html" })
          [ pushListsBench n
          , pushDLBench n
          , pushSeqBench n
          , pushVecBench n
          , indexListsBench m
          , indexSeqsBench m
          , indexVectorsBench m
          ]

