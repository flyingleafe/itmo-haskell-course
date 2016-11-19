{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Homework8.Task1
       ( StateT (..)
       , WriterT (..)
       , ReaderT (..)
       , StackT (..)
       , MaybeT
       , EitherT
       ) where

import           Control.Monad.Trans.Class (MonadTrans (..))
import           Data.Bifunctor            (first)
import           Data.Monoid               ((<>))

-- | Datatypes
newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s)
  }

newtype WriterT w m a = WriterT
  { runWriterT :: m (a, w)
  }

newtype ReaderT r m a = ReaderT
  { runReaderT :: r -> m a
  }

-- | Generalization of things like EitherT, MaybeT, etc.
newtype StackT m2 m1 a = StackT
  { runStackT :: m1 (m2 a)
  }

type EitherT e = StackT (Either e)
type MaybeT = StackT Maybe

-- | StateT instances
instance Functor f => Functor (StateT s f) where
  fmap f a = StateT $ \s -> first f <$> runStateT a s

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  mf <*> ma = StateT $ \s -> do
    (f, s') <- runStateT mf s
    (a, s'') <- runStateT ma s'
    pure (f a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  m >>= f = StateT $ \s -> do
    (a, s') <- runStateT m s
    runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> m >>= \a -> pure (a, s)

-- | WriterT
instance Functor f => Functor (WriterT w f) where
  fmap f a = WriterT $ first f <$> runWriterT a

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure a = WriterT $ pure (a, mempty)
  mf <*> ma = WriterT $ app <$> runWriterT mf <*> runWriterT ma
    where app (f, w) (a, w') = (f a, w <> w')

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return = pure
  ma >>= f = WriterT $ do
    (a, w) <- runWriterT ma
    (b, w') <- runWriterT (f a)
    return (b, w <> w')

instance Monoid w => MonadTrans (WriterT w) where
  lift m = WriterT $ m >>= \a -> pure (a, mempty)

-- | ReaderT
instance Functor f => Functor (ReaderT r f) where
  fmap f a = ReaderT $ fmap f . runReaderT a

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . const . pure
  mf <*> ma = ReaderT $ \r -> runReaderT mf r <*> runReaderT ma r

instance Monad m => Monad (ReaderT r m) where
  return = pure
  m >>= f = ReaderT $ \r -> runReaderT m r >>= flip runReaderT r . f

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

-- | StackT
instance (Functor f1, Functor f2) => Functor (StackT f1 f2) where
  fmap f a = StackT $ fmap f <$> runStackT a

instance (Monad m2, Applicative m1) => Applicative (StackT m1 m2) where
  pure = StackT . pure . pure
  mf <*> ma = StackT $ do
    ef <- runStackT mf
    ea <- runStackT ma
    return $ ef <*> ea

instance Monad m2 => MonadTrans (StackT m2) where
  lift m = StackT $ m >>= pure . pure

-- | EitherT monadic instance
instance Monad m => Monad (EitherT e m) where
  return = pure
  m >>= f = StackT $ runStackT m >>=
    either (pure . Left) (runStackT . f)

-- | MaybeT monadic instance
instance Monad m => Monad (MaybeT m) where
  return = pure
  m >>= f = StackT $ runStackT m >>=
    maybe (pure Nothing) (runStackT . f)
