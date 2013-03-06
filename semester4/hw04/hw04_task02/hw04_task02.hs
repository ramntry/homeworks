module Main where

import Prelude hiding (foldr, foldl)
import Control.Monad
import Data.Foldable
import Data.Monoid
import Test.QuickCheck


data BinaryTree a = Null
                  | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show)

instance Foldable BinaryTree where
    foldMap toMonoid = flip helper mempty
        where helper Null = id
              helper (Node x l r) = helper l . mappend (toMonoid x) . helper r


-- | Testing

findInTree :: (MonadPlus m) => (a -> Bool) -> BinaryTree a -> m a
findInTree p = foldr step mzero
    where step x | p x = mplus (return x)
                 | otherwise = id


binarySearchTreeFromList :: (Ord a) => [a] -> BinaryTree a
binarySearchTreeFromList = foldl insert Null
    where insert Null y = Node y Null Null
          insert tree@(Node x l r) y | y < x = Node x (insert l y) r
                                     | otherwise = Node x l (insert r y)

symorderFlattenTree :: BinaryTree a -> [a]
symorderFlattenTree = flip helper []
    where helper Null = id
          helper (Node x l r) = helper l . (x :) . helper r


type FindFunction m a = (a -> Bool) -> [a] -> m a
type WidePredicate a = a -> a -> Bool

prop_byEqual :: (MonadPlus m, Eq (m Int)) => FindFunction m Int -> [Int] -> Bool
prop_byEqual func l = case l of
    []     -> helper 42 l
    (x:xs) -> helper x xs
    where helper y ys = func (== y) ys == findInTree (== y) (binarySearchTreeFromList ys)

prop_byPredicate :: (MonadPlus m, Eq (m Int)) => WidePredicate Int -> FindFunction m Int -> [Int] -> Bool
prop_byPredicate _ _ [] = True
prop_byPredicate p func (x:xs) = func (p x) (symorderFlattenTree tree) == findInTree (p x) tree
    where tree = binarySearchTreeFromList xs

main = do quickCheckWith stdArgs { maxSize = 20, maxSuccess = 5000 } $ prop_byEqual find
          quickCheckWith stdArgs { maxSize = 20, maxSuccess = 5000 } $ prop_byEqual filter
          quickCheckWith stdArgs { maxSize = 1000, maxSuccess = 500 } $ prop_byPredicate (<) find
          quickCheckWith stdArgs { maxSize = 1000, maxSuccess = 500 } $ prop_byPredicate (>) filter
          quickCheckWith stdArgs { maxSize = 500, maxSuccess = 1000 }
              $ prop_byPredicate (\x y -> x `div` 17 == y `div` 17) filter

