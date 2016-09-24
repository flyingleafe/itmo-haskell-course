{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Homework3.Task4v1 where

import qualified Homework2.Task3 as T
import           Homework3.Task3 (Set)
import qualified Homework3.Task3 as S
import           Homework3.Task4 (Map, MapEntry, Mapped)
import qualified Homework3.Task4 as M

import qualified Data.Foldable   as F

instance Ord a => Set T.Tree a where
  emptySet = mempty
  toList = F.toList
  find = T.find
  insert = T.insert
  delete = T.delete
  next = T.next
  fromList = foldr T.insert S.emptySet

instance Set s (MapEntry k v) => Map s k v where
  emptyMap = S.emptySet
  toList = map M.toPair . S.toList
  find k = fmap M.val' . S.find (M.keyOnly k)
  insert = curry $ S.insert . M.fromPair
  delete = S.delete . M.keyOnly
  next = (fmap M.toPair .) . S.next . M.keyOnly
  fromList = S.fromList . map M.fromPair

instance (Ord k, Eq v) => Eq (Mapped T.Tree k v) where
  m == m' = M.toList m == M.toList m'
