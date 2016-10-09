{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
module Homework4.Task3.FromFish where

import           Homework4.Task3.Classes
import           Prelude                 (id)

instance MonadFish m => Monad m where
  return = returnFish
  x >>= f = (id >=> f) x

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish
  join = id >=> id
