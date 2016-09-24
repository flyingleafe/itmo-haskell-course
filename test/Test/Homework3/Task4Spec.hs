{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Homework3.Task4Spec ( spec ) where

import qualified Homework2.Task3           as T
import qualified Homework3.Task2           ()
import           Homework3.Task4           (MapEntry, Mapped)
import qualified Homework3.Task4           as M
import           Homework3.Task4v1         ()
import           Test.Homework3.Task2Spec  ()

import           Data.Function             (on)
import           Data.List                 (nubBy)
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           (Arbitrary (arbitrary), (===))
import           Test.QuickCheck.Modifiers (OrderedList (..))

instance (Arbitrary k, Arbitrary v) => Arbitrary (MapEntry k v) where
  arbitrary = M.fromPair <$> arbitrary

spec :: Spec
spec = describe "Binary search tree" $
  describe "Map laws" $ do
    let nubPair = nubBy ((==) `on` fst)
    prop "Proper find" $
      \(l :: [(Int, String)], k :: Int) ->
        lookup k (nubPair l) === M.find k (M.fromList l :: Mapped T.Tree Int String)
    prop "Proper insert" $
      \(t :: Mapped T.Tree Int String, k :: Int, v :: String) ->
        M.find k (M.insert k v t) === Just v
    prop "Proper delete" $
      \(t :: Mapped T.Tree Int String, k :: Int) ->
        M.find k (M.delete k t) === Nothing
    prop "Proper toList/fromList" $
      \(l :: OrderedList (Int, String)) ->
        M.toList (M.fromList (getOrdered l) :: Mapped T.Tree Int String) === nubPair (getOrdered l)
