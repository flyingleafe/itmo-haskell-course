{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Homework6.Task1
       ( Tree(..)
       , find
       , insert
       , delete
       , next
       ) where

import           Control.Applicative  ((<|>))
import           Control.Monad.Writer (MonadWriter (..), Writer, tell)
import           Data.Text            (Text)

type TreeWriter = Writer Text

wlog :: MonadWriter Text m => Text -> m ()
wlog t = tell t *> tell "\n"

data Tree k = Tree
  { key   :: k
  , ltree :: Tree k
  , rtree :: Tree k
  }
  | Nil deriving Show

find :: Ord k => k -> Tree k -> TreeWriter (Maybe k)
find _ Nil = wlog "find: Found nothing!" *> pure Nothing
find x Tree {..}
  | x == key  = wlog "find: It's a match!" *> pure (Just key)
  | x < key   = wlog "find: Going to left subtree" *> find x ltree
  | otherwise = wlog "find: Going to right subtree" *> find x rtree

insert :: Ord k => k -> Tree k -> TreeWriter (Tree k)
insert x Nil = wlog "insert: Create a leaf" *> pure (Tree x Nil Nil)
insert x t@Tree{..}
  | x == key  = wlog "insert: Found matching key" *> pure (t { key = x })
  | x < key   = wlog "insert: Going to left subtree" *>
                insert x ltree >>= \lt -> return $ t { ltree = lt }
  | otherwise = wlog "insert: Going to right subtree" *>
                insert x rtree >>= \rt -> return $ t { rtree = rt }

delete :: Ord k => k -> Tree k -> TreeWriter (Tree k)
delete _ Nil = wlog "delete: Empty tree, nothing to delete!" *> pure Nil
delete x t@Tree{..}
  | x == key  = wlog "delete: Found match, start root deletion" *> deleteRoot t
  | x < key   = wlog "delete: Going to left subtree" *>
                delete x ltree >>= \lt -> return $ t { ltree = lt }
  | otherwise = wlog "delete: Going to right subtree" *>
                delete x rtree >>= \rt -> return $ t { rtree = rt }

deleteRoot :: Tree k -> TreeWriter (Tree k)
deleteRoot Nil = wlog "deleteRoot: It's a Nil, wtf?" *> pure Nil
deleteRoot Tree { ltree = Nil, rtree = Nil } =
  wlog "deleteRoot: It's a leaf, deleting!" *> pure Nil
deleteRoot Tree { rtree = Nil, .. } =
  wlog "deleteRoot: Has only left subtree, returning it" *> pure ltree
deleteRoot Tree { ltree = Nil, .. } =
  wlog "deleteRoot: Has only right subtree, returning it" *> pure rtree
deleteRoot Tree {..} =
  wlog "deleteRoot: Has both subtrees, pick leftmost node in right subtree " *>
  pickLeftmost rtree >>= \(y, rtree') -> return $ Tree y ltree rtree'
  where pickLeftmost Nil = error "non possible"
        pickLeftmost (Tree k Nil rt) =
          wlog "pickLeftmost: Found leftmost node" *> pure (k, rt)
        pickLeftmost (Tree k lt rt) =
          wlog "pickLeftmost: Going to left subtree" *> pickLeftmost lt >>=
          \(y', lt'') -> return (y', Tree k lt'' rt)

next :: Ord k => k -> Tree k -> TreeWriter (Maybe k)
next _ Nil = wlog "next: Empty tree!" *> pure Nothing
next x Tree{..}
  | x < key = wlog "next: Trying to go left" *>
              next x ltree >>= \nxt -> return $ nxt <|> Just key
  | otherwise = wlog "next: Going to right subtree" *> next x rtree
