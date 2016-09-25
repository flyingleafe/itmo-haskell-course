{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}
module Homework3.Task4v2 where

import qualified Homework2.Task3 as T
import           Homework3.Task3 (Set)
import qualified Homework3.Task3 as S
import           Homework3.Task4 (Map, MapEntry, Mapped)
import qualified Homework3.Task4 as M

import qualified Data.Foldable   as F

instance Ord k => Map T.Tree k v where
  emptyMap = mempty
  toList = map M.toPair . F.toList
  find k = fmap M.val' . T.find (M.keyOnly k)
  insert = curry $ T.insert . M.fromPair
  delete = T.delete . M.keyOnly
  next = (fmap M.toPair . ) . T.next . M.keyOnly
  fromList = foldr (T.insert . M.fromPair) mempty

instance Functor T.Tree where
  fmap _ T.Nil      = T.Nil
  fmap f T.Tree{..} = T.Tree (f key) height (fmap f ltree) (fmap f rtree)

keyUnit :: k -> MapEntry k ()
keyUnit k = M.fromPair (k, ())

instance Ord k => Set T.Tree k where
  emptySet = M.key' <$> M.emptyMap
  toList = map fst . M.toList . fmap keyUnit
  find k = fmap (const k) . M.find k . fmap keyUnit
  insert k = fmap M.key' . M.insert k () . fmap keyUnit
  delete k = fmap M.key' . M.delete k . fmap keyUnit
  next k = fmap fst . M.next k . fmap keyUnit
  fromList = fmap M.key' . M.fromList . fmap (\k -> (k, ()))

instance (Ord k, Eq v) => Eq (Mapped T.Tree k v) where
  m == m' = M.toList m == M.toList m'
