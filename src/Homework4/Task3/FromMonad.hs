{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
module Homework4.Task3.FromMonad where

import           Homework4.Task3.Classes
import           Prelude                 (id)

instance Monad m => MonadFish m where
  returnFish = return
  (f >=> g) x = f x >>= g

instance Monad m => MonadJoin m where
  returnJoin = return
  join x = x >>= id
