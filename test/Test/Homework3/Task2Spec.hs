{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Homework3.Task2Spec ( spec ) where

import           Homework3.Task2
import           Homework3.Task3       (fromList)
import           Homework3.Task4v1     ()
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (arbitrary), (===))

instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = fromList <$> arbitrary

spec :: Spec
spec = describe "Binary search tree" $
  describe "Monoid laws" $ do
    prop "Left identity" $
      \(a :: Tree Int) -> mappend mempty a === a
    prop "Right identity" $
      \(a :: Tree Int) -> mappend a mempty === a
    prop "Associativity" $
      \(x, y, z :: Tree Int) -> mappend x (mappend y z) === mappend (mappend x y) z
