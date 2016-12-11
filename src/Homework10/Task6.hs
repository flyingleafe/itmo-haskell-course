{-# LANGUAGE TemplateHaskell #-}

module Homework10.Task6
       ( FS (..)
       , HasFS (..)
       , getDirectory
       , treeToFs
       , fsToTree
       ) where

import           Control.Lens     (makeClassy)
import           Data.Tree        (Tree (..))
import           System.File.Tree (FSTree (..), getDirectory')

data FS = Dir  { _name     :: FilePath
               , _contents :: [FS]
               }
        | File { _name     :: FilePath
               }
        deriving Show

makeClassy ''FS

treeToFs :: Tree FilePath -> FS
treeToFs (Node nm [])   = File nm
treeToFs (Node nm subs) = Dir nm $ map treeToFs subs

fsToTree :: FS -> Tree FilePath
fsToTree (File nm)      = Node nm []
fsToTree (Dir nm conts) = Node nm $ map fsToTree conts

fstreeToFs :: FSTree -> FS
fstreeToFs = treeToFs . toTree

getDirectory :: FilePath -> IO FS
getDirectory = fmap fstreeToFs . getDirectory'
