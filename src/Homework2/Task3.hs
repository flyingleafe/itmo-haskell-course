{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Homework2.Task3
  ( Tree(..)
  , find
  , insert
  , delete
  , next
  ) where

import           Control.Applicative ((<|>))

data Tree k = Tree { key    :: k
                   , height :: Int
                   , ltree  :: Tree k
                   , rtree  :: Tree k
                   }
            | Nil deriving Show

makeTree :: k -> Tree k -> Tree k -> Tree k
makeTree x l r = Tree x h l r
  where h = max (height l) (height r) + 1

{--
rotateLeft, rotateRight, bigRotateLeft, bigRotateRight :: Tree k -> Tree k
rotateLeft Nil = error "Invalid left rotation"
rotateLeft Tree { rtree = Nil } = error "Invalid left rotation"
rotateLeft (Tree a _ lt rt) = makeTree (key rt) (makeTree a lt $ ltree rt) $ rtree rt

rotateRight Nil = error "Invalid right rotation"
rotateRight Tree { ltree = Nil } = error "Invalid right rotation"
rotateRight (Tree a _ lt rt) = makeTree (key lt) (ltree lt) $ makeTree a (rtree lt) rt

bigRotateLeft Nil = error "Invalid big left rotation"
bigRotateLeft t@Tree { rtree = rt } = rotateLeft $ t { rtree = rotateRight rt }

bigRotateRight Nil = error "Invalid big right rotation"
bigRotateRight t@Tree { ltree = lt } = rotateRight $ t { ltree = rotateLeft lt }

height' :: Tree k -> Int
height' Nil = 0
height' t   = height t

rebalance :: Tree k -> Tree k
rebalance Nil = Nil
--}

find :: Ord k => k -> Tree k -> Maybe k
find _ Nil = Nothing
find x Tree{..}
  | x == key = return key
  | x < key = find x ltree
  | otherwise = find x rtree

insert :: Ord k => k -> Tree k -> Tree k
insert x Nil = Tree x 1 Nil Nil
insert x t@Tree{..}
  | x == key = t { key = x }  -- in case of exotic Eq instance
  | x < key = t { ltree = insert x ltree }
  | otherwise = t { rtree = insert x rtree }

delete :: Ord k => k -> Tree k -> Tree k
delete _ Nil = Nil
delete x t@Tree{..}
  | x == key = deleteRoot t
  | x < key = t { ltree = delete x ltree }
  | otherwise = t { rtree = delete x rtree }

deleteRoot :: Tree k -> Tree k
deleteRoot Nil = Nil
deleteRoot Tree { ltree = Nil, rtree = Nil } = Nil
deleteRoot Tree { rtree = Nil, .. } = ltree
deleteRoot Tree { ltree = Nil, .. } = rtree
deleteRoot Tree { .. } = makeTree y ltree rtree'
  where (y, rtree') = pickLeftmost rtree
        pickLeftmost Nil = error "non possible"
        pickLeftmost (Tree k _ Nil rt) = (k, rt)
        pickLeftmost (Tree k _ lt rt) = let (y', lt'') = pickLeftmost lt
                                         in (y', makeTree k lt'' rt)

next :: Ord k => k -> Tree k -> Maybe k
next _ Nil = Nothing
next x Tree{..}
  | x < key = next x ltree <|> return key
  | otherwise = next x rtree
