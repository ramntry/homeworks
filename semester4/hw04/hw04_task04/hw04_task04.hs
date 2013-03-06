module Main where

import Data.List
import Test.QuickCheck

withoutRepetitions :: (Ord a) => [a] -> Bool
withoutRepetitions = all ((== 1) . length) . group . sort

withoutRepetitions1 :: (Eq a) => [a] -> Bool
withoutRepetitions1 [] = True
withoutRepetitions1 (x:xs) = not (x `elem` xs) && withoutRepetitions1 xs

prop_identity :: [Int] -> Bool
prop_identity xs = withoutRepetitions xs == withoutRepetitions1 xs

main = quickCheckWith stdArgs { maxSize = 15, maxSuccess = 5000 } prop_identity
