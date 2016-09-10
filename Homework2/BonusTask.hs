module BonusTask where

import Data.List (minimumBy, deleteBy)
import Data.Ord
import Data.Function

data BinNode a = BinNode { key :: a
                         , children :: BinHeap a
                         , degree :: Int
                         }

type BinHeap a = [BinNode a]

merge :: (Ord a) => BinHeap a -> BinHeap a -> BinHeap a
merge [] bs = bs
merge as [] = as
merge as@(a:as') bs@(b:bs')
  | degree a < degree b = a : merge as' bs
  | degree a > degree b = b : merge as bs'
  | otherwise = mergePair a b : merge as' bs' -- we know for sure that heads of `a'` and `b'` won't be the same degree as `a` and `b`
      where mergePair x y = if key x < key y
                            then y `becomesSonOf` x
                            else x `becomesSonOf` y
            x `becomesSonOf` y = y { children = x : children y
                                   , degree = degree y + 1
                                   }

getMinimum :: (Ord a) => BinHeap a -> a
getMinimum = minimum . map key

insert :: (Ord a) => a -> BinHeap a -> BinHeap a
insert = merge . singleton

singleton :: a -> BinHeap a
singleton a = [BinNode { key = a
                       , children = []
                       , degree = 0
                       }]

extractMin :: (Ord a) => BinHeap a -> (a, BinHeap a)
extractMin [] = error "empty heap"
extractMin hs = (key minNode, children minNode `merge` hs')
  where minNode = minimumBy (comparing key) hs
        hs' = deleteBy ((==) `on` key) minNode hs


fromList :: (Ord a) => [a] -> BinHeap a
fromList = foldr insert []

toList :: BinHeap a -> [a]
toList = concatMap $ \x -> key x : toList (children x)
