{-# LANGUAGE RankNTypes #-}

module Homework10.Task10
       ( iso
       , from
       , fsIso
       ) where

import           Control.Applicative (Const (..))
import           Control.Lens        (Profunctor (..))
import           Data.Tagged         (Tagged (..))
import           Data.Tree           (Tree (..))

import           Homework10.Task6

type Iso b a = forall p f . (Profunctor p, Functor f) => p a (f a) -> p b (f b)

iso :: (b -> a) -> (a -> b) -> Iso b a
iso fba fab = dimap fba $ fmap fab

from :: Iso b a -> Iso a b
from is =
  let fab = unTagged $ is (Tagged id)
      fba = getConst . is Const
  in iso fab fba

fsIso :: Iso FS (Tree FilePath)
fsIso = iso fsToTree treeToFs
