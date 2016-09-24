{-# LANGUAGE FlexibleInstances #-}
module Homework3.Task1 where

import           Data.Monoid ((<>))

data Coin color = Coin { getCoin :: Int }

data Red
data Blue

blue :: Blue
blue = undefined

red :: Red
red  = undefined

createCoins :: color -> Int -> Coin color
createCoins _ = Coin

addCoins :: Coin color -> Coin color -> Coin color
addCoins (Coin a) (Coin b) = Coin (a + b)

class Color a where
  importance :: Coin a -> Double     -- Double is chosen to be able to easily add new colors between existing

instance Color Blue where
  importance = const 0

instance Color Red where
  importance = const 1

instance Monoid (Coin color) where
  mempty = Coin 0
  mappend = addCoins

compareCoins :: (Color a, Color b) => Coin a -> Coin b -> Ordering
compareCoins a b = importance a `compare` importance b <>
                   getCoin a `compare` getCoin b
