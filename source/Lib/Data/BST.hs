module Lib.Data.BST
  ( BST,
    singleton,
    insert,
    Lib.Data.BST.elem,
    fromList,
    findByLevel,
  )
where

data BST a = Nil | Node a (BST a) (BST a)

singleton :: a -> BST a
singleton x = Node x Nil Nil

insert :: (Ord a) => a -> BST a -> BST a
insert x Nil = singleton x
insert x (Node a leftTree rightTree)
  | x < a = Node a (insert x leftTree) rightTree
  | x > a = Node a leftTree (insert x rightTree)
  | otherwise = Node x leftTree rightTree

elem :: (Ord a) => a -> BST a -> Bool
_ `elem` Nil = False
x `elem` (Node a leftTree rightTree)
  | x < a = x `Lib.Data.BST.elem` leftTree
  | x > a = x `Lib.Data.BST.elem` rightTree
  | otherwise = True

fromList :: (Ord a) => [a] -> BST a
fromList = foldl (flip insert) Nil

findByLevel :: Int -> BST a -> [a]
findByLevel _ Nil = []
findByLevel 0 (Node x _ _) = [x]
findByLevel level (Node _ leftTree rightTree) =
  let findInSubtree = findByLevel (level - 1)
   in findInSubtree leftTree ++ findInSubtree rightTree
