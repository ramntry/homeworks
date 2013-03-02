module Main where

data BinaryTree a = EmptyBinaryTree
                  | BinaryTreeNode a (BinaryTree a) (BinaryTree a)
    deriving (Show)

binarySearchTreeFromList :: (Ord) a => [a] -> BinaryTree a
binarySearchTreeFromList = foldl insert EmptyBinaryTree
    where insert EmptyBinaryTree                    x             = BinaryTreeNode x EmptyBinaryTree EmptyBinaryTree
          insert node@(BinaryTreeNode r left right) x | x < r     = BinaryTreeNode r (insert left x) right
                                                      | x > r     = BinaryTreeNode r left (insert right x)
                                                      | otherwise = node

binaryTreeHeight :: BinaryTree a -> Int
binaryTreeHeight EmptyBinaryTree               = 0
binaryTreeHeight (BinaryTreeNode r left right) = 1 + max (binaryTreeHeight left) (binaryTreeHeight right)
