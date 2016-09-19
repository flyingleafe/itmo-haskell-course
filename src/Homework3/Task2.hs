{-# OPTIONS_GHC -fno-warn-orphans #-}
module Homework3.Task2 ( module Exports ) where

import           Homework2.Task3 as Exports

instance Ord a => Eq (Tree a) where
  t == t' = toList t == toList t'

instance Foldable Tree where
  foldr _ x Nil            = x
  foldr f x (Tree y lt rt) = foldr f (f y $ foldr f x rt) lt

instance Ord a => Monoid (Tree a) where
  mempty = Nil
  mappend = foldr insert
