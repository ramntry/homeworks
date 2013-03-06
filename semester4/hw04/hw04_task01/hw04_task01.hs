module Main where

import Control.Monad
import Data.List
import Test.QuickCheck

data BinaryTree a = Null
                  | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show)

findInTree :: (a -> Bool) -> BinaryTree a -> Maybe a
findInTree _ Null = Nothing
findInTree p (Node x l r) | p x = Just x
                          | otherwise = findInTree p l `mplus` findInTree p r


-- | Testing

binarySearchTreeFromList :: (Ord a) => [a] -> BinaryTree a
binarySearchTreeFromList = foldl insert Null
    where insert Null y = Node y Null Null
          insert tree@(Node x l r) y | y < x = Node x (insert l y) r
                                     | y > x = Node x l (insert r y)
                                     | otherwise = tree

preorderFlattenTree :: BinaryTree a -> [a]
preorderFlattenTree = flip helper []
    where helper Null = id
          helper (Node x l r) = (x :) . helper l . helper r

prop_byEqual :: [Int] -> Bool
prop_byEqual l = case l of
    []     -> helper 42 l
    (x:xs) -> helper x xs
    where helper y ys = find (== y) ys == findInTree (== y) (binarySearchTreeFromList ys)

prop_byPredicate :: (Int -> Int -> Bool) -> [Int] -> Bool
prop_byPredicate _ [] = True
prop_byPredicate p (x:xs) = find (p x) (preorderFlattenTree tree) == findInTree (p x) tree
    where tree = binarySearchTreeFromList xs

main = do
    quickCheckWith stdArgs { maxSize = 10, maxSuccess = 5000 } prop_byEqual
    quickCheckWith stdArgs { maxSize = 1000, maxSuccess = 500 } $ prop_byPredicate (<)
    quickCheckWith stdArgs { maxSize = 1000, maxSuccess = 500 } $ prop_byPredicate (>)
    quickCheckWith stdArgs { maxSize = 500, maxSuccess = 1000 } $ prop_byPredicate (\x y -> x `div` 17 == y `div` 17)
