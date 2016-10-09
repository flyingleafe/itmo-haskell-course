{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Homework5.Task1 where

import           Prelude (($))

class Functor f where
  fmap :: (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b

class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

data Identity a = Identity a
data Either a b = Left a | Right b
data Tree a = Nil | Tree a (Tree a) (Tree a)
data Const a b = Const a

-- Identity
instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> x = fmap f x

instance Foldable Identity where
  foldr f b (Identity a) = f a b

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity $ f a

-- Either
instance Functor (Either a) where
  fmap f (Right b) = Right $ f b
  fmap _ (Left a)  = Left a

instance Applicative (Either a) where
  pure = Right
  Right f <*> x = fmap f x
  Left a <*> _ = Left a

instance Foldable (Either a) where
  foldr f b (Right a) = f a b
  foldr _ b (Left _)  = b

instance Traversable (Either a) where
  traverse f (Right b) = fmap Right $ f b
  traverse _ (Left a)  = pure $ Left a

-- Tree
instance Functor Tree where
  fmap _ Nil            = Nil
  fmap f (Tree a lt rt) = Tree (f a) (fmap f lt) $ fmap f rt

instance Applicative Tree where
  pure x = Tree x Nil Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Tree f lft rft <*> Tree x lxt rxt = Tree (f x) (lft <*> lxt) $ rft <*> rxt

instance Foldable Tree where
  foldr _ b Nil            = b
  foldr f b (Tree a lt rt) = foldr f (f a $ foldr f b rt) lt

instance Traversable Tree where
  traverse _ Nil            = pure Nil
  traverse f (Tree a lt rt) = Tree <$> f a <*> traverse f lt <*> traverse f rt

-- Const
instance Functor (Const a) where
  fmap _ (Const a) = Const a

-- Applicative instance is impossible!

instance Foldable (Const a) where
  foldr _ b (Const _) = b

instance Traversable (Const a) where
  traverse _ (Const a) = pure $ Const a

-- Pair
instance Functor ((,) a) where
  fmap f (a, b) = (a, f b)

-- Applicative instance is impossible again!

instance Foldable ((,) a) where
  foldr f b (_, a) = f a b

instance Traversable ((,) a) where
  traverse f (a, b) = (,) a <$> f b
