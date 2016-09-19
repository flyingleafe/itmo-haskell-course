module Homework2.Task3 where

data Tree a = Tree a (Tree a) (Tree a) | Nil deriving Show

find :: (Ord a) => a -> Tree a -> Bool
find _ Nil = False
find x (Tree y lt rt)
  | x == y = True
  | x < y = find x lt
  | otherwise = find x rt

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Nil = Tree x Nil Nil
insert x t@(Tree y lt rt)
  | x == y = t
  | x < y = Tree y (insert x lt) rt
  | otherwise = Tree y lt (insert x rt)

delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Nil = Nil
delete x t@(Tree y lt rt)
  | x == y = deleteRoot t
  | x < y = Tree y (delete x lt) rt
  | otherwise = Tree y lt (delete x rt)

deleteRoot :: Tree a -> Tree a
deleteRoot Nil = Nil
deleteRoot (Tree _ Nil Nil) = Nil
deleteRoot (Tree _ lt Nil) = lt
deleteRoot (Tree _ Nil rt) = rt
deleteRoot (Tree _ lt rt) = Tree y lt rt'
  where (y, rt') = pickLeftmost rt
        pickLeftmost Nil = error "non possible"
        pickLeftmost (Tree x Nil rt'') = (x, rt'')
        pickLeftmost (Tree x lt' rt'') = let (y', lt'') = pickLeftmost lt' in (y', Tree x lt'' rt'')

toList :: Tree a -> [a]
toList = toList' []
  where toList' l Nil = l
        toList' l (Tree x lt rt) = toList' l' lt
          where l' = x : toList' l rt

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Nil
