module ControlWork1 where

import           Data.List (foldl', nub)

-- Task 1
leaveStartingWith :: Eq a => a -> [[a]] -> [[a]]
leaveStartingWith x = nub . filter ((x == ) . head)

-- Task 2
countPrimes :: Integral a => [a] -> Int
countPrimes = foldl' countPrime 0
  where countPrime n x = if isPrime x then n + 1 else n
        isPrime y = y > 1 && all (\x -> y `mod` x /= 0) [2..sqrtn y]
          where sqrtn = round . (sqrt :: Double -> Double) . fromIntegral

-- Task 3
data Deque a = Deque { frontLs :: [a]
                     , backLs  :: [a]
                     }

pushFront, pushBack :: a -> Deque a -> Deque a
pushFront x d = d { frontLs = x : frontLs d }
pushBack x d = d { backLs = x : backLs d }

popFront, popBack :: Deque a -> (Maybe a, Deque a)
popFront d@(Deque [] []) = (Nothing, d)
popFront (Deque [] bls) = let (x:xs) = reverse bls
                            in (Just x, Deque xs [])
popFront (Deque (x:xs) bls) = (Just x, Deque xs bls)

popBack d@(Deque [] []) = (Nothing, d)
popBack (Deque fls []) = let (x:xs) = reverse fls
                           in (Just x, Deque [] xs)
popBack (Deque fls (x:xs)) = (Just x, Deque fls xs)

-- Task 3 (my =\)
type Stack a = [a]

stackPop :: Stack a -> (Maybe a, Stack a)
stackPop []     = (Nothing, [])
stackPop (x:xs) = (Just x, xs)

stackPush :: a -> Stack a -> Stack a
stackPush = (:)

data Queue a = Queue { front :: Stack a
                     , back  :: Stack a
                     } deriving Show

pop :: Queue a -> (Maybe a, Queue a)
pop q@(Queue [] []) = (Nothing, q)
pop (Queue [] bls) = let (x:xs) = reverse bls
                     in (Just x, Queue xs [])
pop (Queue (x:xs) bls) = (Just x, Queue xs bls)

push :: a -> Queue a -> Queue a
push x q = q { back = stackPush x $ back q }

-- Task 4
newtype Matrix a = Matrix { getMatrix :: [[a]] }

instance Show a => Show (Matrix a) where
  show = unlines . map (unwords . map show) . getMatrix
