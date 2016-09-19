{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Homework3.Task2 ( spec ) where

import           Homework3.Task2
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (arbitrary), (===))

instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = fromList <$> arbitrary

spec :: Spec
spec = describe "Binary search tree" $
  describe "Monoid laws" $
    prop "Left identity" $
      \(a :: Tree Int) -> mappend mempty a === a
