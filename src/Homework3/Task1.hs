{-# LANGUAGE FlexibleInstances #-}
module Homework3.Task1 where

import           Data.Function (on)
import           Data.Monoid   ((<>))

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
  importance :: Coin a -> Int

instance Color Blue where
  importance = const 0

instance Color Red where
  importance = const 1

instance Monoid (Coin color) where
  mempty = Coin 0
  mappend = addCoins

instance Color color => Eq (Coin color) where
  a == b = importance a == importance b && getCoin a == getCoin b

instance Color color => Ord (Coin color) where
  compare = (compare `on` importance) <> (compare `on` getCoin)

expr :: Bool
expr = createCoins blue 10 < createCoins red 5
