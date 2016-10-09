{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
module Homework4.Task3.FromJoin where

import           Homework4.Task3.Classes

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance (Functor m, MonadJoin m) => Monad m where
  return = returnJoin
  x >>= f = join (fmap f x)

instance (Functor m, MonadJoin m) => MonadFish m where
  returnFish = returnJoin
  (f >=> g) x = join (fmap g (f x))
