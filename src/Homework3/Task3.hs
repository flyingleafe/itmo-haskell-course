{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Homework3.Task3 ( Set(..) ) where

import           Homework3.Task2 ()

class Set s a where
  emptySet :: s a
  toList :: s a -> [a]
  find :: a -> s a -> Maybe a
  insert :: a -> s a -> s a
  delete :: a -> s a -> s a
  next :: a -> s a -> Maybe a
  fromList :: [a] -> s a
