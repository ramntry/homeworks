module Main where

import Test.QuickCheck

pairProduct :: Int -> [Int]
pairProduct n = l >>= \x -> l >>= \y -> return (x * y)
    where l = [1..n]

prop_size n = n >= 0 ==> length (pairProduct n) == n * n
prop_sum n = n >= 0 ==> sum (pairProduct n) == n^2 * (n + 1)^2 `div` 4

main = do
    quickCheckWith stdArgs { maxSize = 15 } prop_size
    quickCheckWith stdArgs { maxSize = 15 } prop_sum
