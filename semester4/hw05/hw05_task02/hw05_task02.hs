module Main where

import Test.QuickCheck

myAll :: (a -> Bool) -> [a] -> Bool
myAll = all

myAll2 :: (a -> Bool) -> [a] -> Bool
myAll2 p = foldr ((&&) . p) True

main = quickCheck ((\l -> myAll even l == myAll2 even l) :: [Int] -> Bool)
