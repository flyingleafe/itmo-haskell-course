module Homework4.Task4 ( State(..) ) where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f st = State $ \s -> let (a, s') = runState st s
                            in (f a, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  sf <*> sx = State $ \s -> let (f, s') = runState sf s
                            in runState (fmap f sx) s'

instance Monad (State s) where
  sx >>= f = State $ \s -> let (x, s') = runState sx s
                           in runState (f x) s'
