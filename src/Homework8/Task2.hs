{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Homework8.Task2
       (
       ) where

import           Control.Monad.State.Class (MonadState (..))

import           Homework8.Task1           (ReaderT (..), StackT (..),
                                            StateT (..))

instance Monad m => MonadState s (StateT s m) where
  get = StateT $ \s -> pure (s, s)
  put s = StateT $ const $ pure ((), s)

instance MonadState s m => MonadState s (ReaderT r m) where
  get = ReaderT $ const get
  put = ReaderT . const . put

instance (Applicative m1, Monad (StackT m1 m2), MonadState s m2) =>
         MonadState s (StackT m1 m2) where
  get = StackT $ pure <$> get
  put s = StackT $ pure <$> put s
