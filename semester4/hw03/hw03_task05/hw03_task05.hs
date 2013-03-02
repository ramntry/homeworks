module Main where

import Test.QuickCheck
import Data.Maybe

data CrownedTree a = Leaf a
                   | Branch (CrownedTree a) (CrownedTree a)
    deriving (Show)

orderedCrownedTreeFromList :: [a] -> CrownedTree a
orderedCrownedTreeFromList = foldl1 Branch . map fst . foldr push []
    where push x ((fstTree, fstSize) : (sndTree, sndSize) : rest)
                 | fstSize == sndSize = push x ((Branch fstTree sndTree, 2 * fstSize) : rest)
          push x stack = (Leaf x, 1) : stack

safeOrderedCrownedTreeFromList :: [a] -> Maybe (CrownedTree a)
safeOrderedCrownedTreeFromList [] = Nothing
safeOrderedCrownedTreeFromList xs = Just (orderedCrownedTreeFromList xs)

crownedTreeHeight :: CrownedTree a -> Int
crownedTreeHeight (Leaf _) = 0
crownedTreeHeight (Branch left right) = 1 + max (crownedTreeHeight left) (crownedTreeHeight right)

minPathLength :: CrownedTree a -> Int
minPathLength (Leaf _) = 0
minPathLength (Branch left right) = 1 + min (crownedTreeHeight left) (crownedTreeHeight right)

prop_height :: [Int] -> Property
prop_height l = not (null l) ==> (crownedTreeHeight . orderedCrownedTreeFromList) l
                                 <= (ceiling . logBase 2 . fromIntegral . length) l

main = quickCheck prop_height
