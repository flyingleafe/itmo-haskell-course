{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}

module Homework10.Task7
       ( cd
       , count
       , hasChild
       , ls
       , file
       ) where

import           Control.Lens

import           Homework10.Task6

cd :: HasFS a => FilePath -> Traversal' a FS
cd dir = contents . each . filtered foo
  where foo d = d ^. name == dir

count :: Fold a b -> Getter a Int
count t = partsOf t . to length

hasChild :: HasFS a => FilePath -> Getter a Bool
hasChild fp = count (cd fp) . to (/= 0)

ls :: HasFS a => Traversal' a FilePath
ls = contents . each . name

file :: HasFS a => FilePath -> Traversal' a FilePath
file f = cd f . name
