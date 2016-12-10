{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Homework9.Task1
       ( STVector
       , getVecSize
       , fromList
       , pushBack
       , popBack
       ) where

import           Control.DeepSeq   (NFData)
import           Control.Monad     (forM_, when)
import           Control.Monad.ST
import           Data.Array.Base   (getNumElements, unsafeRead, unsafeWrite)
import           Data.Array.MArray (MArray (..), newListArray, readArray, writeArray)
import           Data.Array.ST     (STArray)
import           Data.Ix           (Ix (..))
import           Data.STRef        (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import           GHC.Generics      (Generic)

data STVector s i e = STVector
  { vecSize :: STRef s Int
  , vecData :: STRef s (STArray s i e)
  } deriving (Generic)

instance NFData (STVector s i e)

instance MArray (STVector s) a (ST s) where
  getNumElements = readSTRef . vecSize
  getBounds STVector {..} = readSTRef vecData >>= getBounds
  unsafeRead STVector {..} idx = readSTRef vecData >>= flip unsafeRead idx
  unsafeWrite STVector {..} idx val = readSTRef vecData >>=
                                      \arr -> unsafeWrite arr idx val
  newArray bnds val = STVector
                      <$> newSTRef (rangeSize bnds)
                      <*> (newArray bnds val >>= newSTRef)

getVecSize :: STVector s Int e -> ST s Int
getVecSize = getNumElements

-- get and put element by index -- readArray & writeArray

-- toList = getElems

fromList :: [e] -> ST s (STVector s Int e)
fromList ls = newListArray (0, length ls) ls

pushBack :: STVector s Int e -> e -> ST s ()
pushBack vec@STVector {..} val = do
  oldData <- readSTRef vecData
  cap <- rangeSize <$> getBounds oldData
  size <- readSTRef vecSize
  when (size == cap) $ do
    let newSize = size * 2
    newData <- newArray_ (0, newSize)
    forM_ [0..size - 1] $ \i ->
      readArray oldData i >>= writeArray newData i
    writeSTRef vecData newData
  modifySTRef' vecSize (+1)
  writeArray vec size val

popBack :: STVector s Int e -> ST s e
popBack STVector {..} = do
  size <- readSTRef vecSize
  res <- flip readArray size =<< readSTRef vecData
  modifySTRef' vecSize $ \k -> k - 1
  return res
