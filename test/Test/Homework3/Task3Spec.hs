{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Homework3.Task3Spec ( spec ) where

import qualified Homework2.Task3           as T
import qualified Homework3.Task2           ()
import qualified Homework3.Task3           as S
import           Homework3.Task4v1         ()
import           Test.Homework3.Task2Spec  ()

import           Data.List                 (nub, sort)
import           Data.Maybe                (isJust)
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           ((===))
import           Test.QuickCheck.Modifiers (OrderedList (..))

nextLs :: Ord a => a -> OrderedList a -> Maybe a
nextLs x = nx . getOrdered
  where nx [] = Nothing
        nx (y:ys)
          | x < y = Just y
          | otherwise = nx ys

spec :: Spec
spec = describe "Binary search tree" $
  describe "Set laws" $ do
    prop "Proper find" $
      \(t :: T.Tree Int, a :: Int) -> isJust (S.find a t) === elem a (S.toList t)
    prop "Proper insertion" $
      \(t :: T.Tree Int, a :: Int) -> S.find a (S.insert a t) === Just a
    prop "Proper deletion" $
      \(t :: T.Tree Int, a :: Int) -> S.find a (S.delete a t) === Nothing
    prop "Proper toList/fromList" $
      \(l :: [Int]) -> S.toList (S.fromList l :: T.Tree Int) === nub (sort l)
    prop "Proper next" $
      \(l :: OrderedList Int, x :: Int) -> nextLs x l === S.next x (S.fromList $ getOrdered l :: T.Tree Int)
