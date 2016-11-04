{-# OPTIONS_GHC -fno-warn-orphans #-}
module Homework3.Task2 () where

import           Data.Foldable   (toList)
import           Homework2.Task3

instance Foldable Tree where
  foldr _ x Nil              = x
  foldr f x (Tree y _ lt rt) = foldr f (f y $ foldr f x rt) lt

instance Ord k => Eq (Tree k) where
  t == t' = toList t == toList t'

instance Ord k => Monoid (Tree k) where
  mempty = Nil
  mappend = foldr insert
