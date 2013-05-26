module Main where

import Data.Maybe
import Data.List
import Test.QuickCheck

data BST a = Empty
           | Node a (BST a) (BST a)
    deriving (Show)

add :: (Ord a) => BST a -> a -> BST a
add Empty x = Node x Empty Empty
add node@(Node r left right) x | x < r = Node r (add left x) right
                               | x > r = Node r left (add right x)
                               | otherwise = node

extractMin :: (Ord a) => BST a -> Maybe (a, BST a)
extractMin Empty = Nothing
extractMin (Node x Empty right) = Just (x, right)
extractMin (Node x left right) = Just (minInLeft, Node x leftWithoutMin right)
    where (minInLeft, leftWithoutMin) = fromJust $ extractMin left

remove :: (Ord a) => BST a -> a -> BST a
remove Empty _ = Empty
remove node@(Node r left right) x | x < r = Node r (remove left x) right
                                  | x > r = Node r left (remove right x)
                                  | otherwise = removeRoot node
    where removeRoot (Node x Empty Empty) = Empty
          removeRoot (Node x child Empty) = child
          removeRoot (Node x Empty child) = child
          removeRoot (Node x right left) = Node minInRight left rightWithoutMin
          (minInRight, rightWithoutMin) = fromJust $ extractMin right

removes :: (Ord a) => BST a -> [a] -> BST a
removes = foldl remove

has :: (Ord a) => BST a -> a -> Bool
has Empty _ = False
has (Node r left right) x = r == x || has left x || has right x

size :: (Ord a) => BST a -> Int
size Empty = 0
size (Node _ left right) = size left + size right + 1

height :: (Ord a) => BST a -> Int
height Empty = 0
height (Node _ left right) = max (height left) (height right) + 1

fromList :: (Ord a) => [a] -> BST a
fromList = foldl add Empty

toList :: (Ord a) => BST a -> [a]
toList = toListHelper []
    where toListHelper acc Empty = acc
          toListHelper acc (Node x left right) = toListHelper (x : toListHelper acc right) left

unique :: (Ord a) => [a] -> [a]
unique = map head . group . sort

uniqueByBST :: (Ord a) => [a] -> [a]
uniqueByBST = toList . fromList

prop_add :: [Int] -> Bool
prop_add xs = unique xs == uniqueByBST xs

prop_remove :: [Int] -> [Int] -> Bool
prop_remove lhs rhs = toList (removes (fromList lhs) rhs) == unique lhs \\ rhs

main = do
    quickCheck prop_add
    quickCheck prop_remove
