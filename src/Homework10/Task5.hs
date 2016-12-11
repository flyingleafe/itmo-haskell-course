{-# LANGUAGE RankNTypes #-}

module Homework10.Task5 () where

import           Data.Functor.Identity (runIdentity)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

(.~) :: Lens' s a -> a -> s -> s
ln .~ v = runIdentity . ln (const $ pure v)

(^.) :: s -> Lens' s a -> a
s ^. ln = fst $ ln (\a -> (a, a)) s

(%~) :: Lens' s a -> (a -> a) -> s -> s
ln %~ f = runIdentity . ln (pure . f)
