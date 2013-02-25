module Main where

import Test.QuickCheck

customReverse :: [a] -> [a]
customReverse = foldl (flip (:)) []

main = quickCheck ((\xs -> reverse xs == customReverse xs) :: [Int] -> Bool)
