module Homework4.Task1 where

import           Control.Applicative ((<|>))
import           Data.Maybe          (maybeToList)

data InNode a = Node { label :: a, parent :: Maybe (InNode a) }

split :: [a] -> Maybe (a, [a])
split []     = Nothing
split (x:xs) = Just (x, xs)

leastCommonAncestor :: Eq a => InNode a -> InNode a -> Maybe (InNode a)
leastCommonAncestor a b = lastEqNode (reverse alist) (reverse blist)
  where alist = a : ancestors a
        blist = b : ancestors b
        lastEqNode al bl = split al >>=
                           \(x, xs) -> split bl >>=
                           \(y, ys) -> if label x == label y
                                       then lastEqNode xs ys <|> return x
                                       else Nothing

ancestors :: InNode a -> [InNode a]
ancestors a = maybeToList (parent a) >>= \pa -> pa : ancestors pa

instance Show a => Show (InNode a) where
  show a = "Node " ++ show (label a)

-- for testing
tn1, tn2, tn3, tn4, tn5, tn6, tn7, tn8, tn9, tn10, tn11, tn12 :: InNode Int
tn1 = Node 1 Nothing
tn2 = Node 2 $ Just tn1
tn3 = Node 3 $ Just tn1
tn4 = Node 4 $ Just tn1
tn5 = Node 5 $ Just tn3
tn6 = Node 6 $ Just tn3
tn7 = Node 7 $ Just tn2
tn8 = Node 8 $ Just tn7
tn9 = Node 9 $ Just tn7
tn10 = Node 10 $ Just tn5
tn11 = Node 11 $ Just tn2
tn12 = Node 12 Nothing
