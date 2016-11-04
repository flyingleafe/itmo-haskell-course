module State where

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  x >>= f = State $ \s -> let (x', s') = runState x s
                          in runState (f x') s'
