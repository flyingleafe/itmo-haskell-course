{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Homework10.Task8
       ( AnnotatedFS (..)
       , getPath
       , lsAll
       , files
       , dirs
       , ext
       , changeExts
       , delEmptySubdir
       , move
       ) where

import           Control.Lens
import           Data.List        (elemIndex)
import           Data.Maybe       (fromMaybe)

import           Homework10.Task6
import           Homework10.Task7

data AnnotatedFS = AFS
  { _prevPath :: FilePath
  , _fs       :: FS
  } deriving Show

makeLenses ''AnnotatedFS

instance HasFS AnnotatedFS where
  fS = fs
  name = fs . name
  contents = fs . contents

class HasPath a where
  getPath :: Getter a FilePath

instance HasPath FS where
  getPath = name

(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ (if null a then "" else "/") ++ b

instance HasPath AnnotatedFS where
  getPath = merge (</>) prevPath name

combine :: Traversal' a b -> Traversal' a b -> Traversal' a b
combine t1 t2 f s = t1 f s *> t2 f s

merge :: (a -> b -> c) -> Getter m a -> Getter m b -> Getter m c
merge foo g1 g2 = to $ foo <$> view g1 <*> view g2

lsAll :: HasFS a => Traversal' a FilePath
lsAll = contents . each . combine name lsAll

isFile :: FS -> Bool
isFile (File _) = True
isFile _        = False

files :: HasFS a => Traversal' a FS
files = contents . each . filtered isFile

dirs :: HasFS a => Traversal' a FS
dirs = contents . each . filtered (not . isFile)

ext :: Lens' FilePath String
ext f fname = (base ++) <$> f x
  where idx = fromMaybe (length fname) $ elemIndex '.' fname
        (base, x) = splitAt idx fname

changeExts :: String -> FS -> FS
changeExts s = files.name.ext .~ s

delEmptySubdir :: HasFS a => FilePath -> Getter a a
delEmptySubdir fp = to (contents %~ delIt)
  where delIt = filter $ \f ->
          not (isFile f) && f ^. name == fp && lengthOf contents f == 0

move :: (HasFS a, HasPath a) => FilePath -> Getter a AnnotatedFS
move fp = merge AFS getPath (singular $ cd fp)
