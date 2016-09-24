{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Homework3.Task4
  ( T.Tree
  , Map(..)
  , MapEntry(..)
  , toPair
  , fromPair
  , keyOnly
  , Mapped
  ) where

import           Data.Function   (on)
import qualified Homework2.Task3 as T

data MapEntry k v = Entry { key' :: k, val' :: v } deriving Show

instance Eq k => Eq (MapEntry k v) where
  m == n = key' m == key' n

instance Ord k => Ord (MapEntry k v) where
  compare = compare `on` key'

toPair :: MapEntry k v -> (k, v)
toPair e = (key' e, val' e)

fromPair :: (k, v) -> MapEntry k v
fromPair = uncurry Entry

keyOnly :: k -> MapEntry k v
keyOnly k = Entry k undefined

type Mapped s k v = s (MapEntry k v)

class Map s k v where
  emptyMap :: Mapped s k v
  toList :: Mapped s k v -> [(k, v)]
  find :: k -> Mapped s k v -> Maybe v
  insert :: k -> v -> Mapped s k v -> Mapped s k v
  delete :: k -> Mapped s k v -> Mapped s k v
  next :: k -> Mapped s k v -> Maybe (k, v)
  fromList :: [(k, v)] -> Mapped s k v
