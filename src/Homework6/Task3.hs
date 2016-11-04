{-# LANGUAGE TemplateHaskell #-}
module Homework6.Task3
       ( moving
       )where

import           Control.Lens        hiding (_head)
import           Control.Monad.State (State, evalState)
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NE

data SMA = SMA
  { _offset   :: Int
  , _lastSMA  :: Double
  , _prevVals :: NonEmpty Double
  }

makeLenses ''SMA

type SMAState = State SMA

neTail :: NonEmpty a -> NonEmpty a
neTail ne = case NE.tail ne of
  (x:xs) -> x :| xs
  []     -> error "Empty tail!"

neHead :: Lens' (NonEmpty a) a
neHead f (x :| xs) = (:| xs) <$> f x

smaTail :: Int -> [Double] -> SMAState [Double]
smaTail _ [] = pure []
smaTail n (x:xs) =
  use offset >>= \off ->
  use lastSMA >>= \ls ->
  use (prevVals.neHead) >>= \pv ->
  (if off < n
   then return $ (ls * fromIntegral off + x) / fromIntegral (off + 1)
   else prevVals %= neTail >>
        return (ls - pv / fromIntegral n + x / fromIntegral n)) >>= \nls ->
  lastSMA .= nls >>
  offset += 1 >>
  (nls :) <$> smaTail n xs

moving :: Int -> [Double] -> [Double]
moving _ []     = []
moving n (x:xs) = x : evalState (smaTail n xs) (SMA 1 x (x :| xs))
