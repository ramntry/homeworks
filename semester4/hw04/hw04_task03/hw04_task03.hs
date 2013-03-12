module Main where

import Test.QuickCheck

countEvens :: (Integral a) => [a] -> Int
countEvens = foldr step 0
    where step x | even x = (+ 1)
                 | otherwise = id

countEvens2 :: (Integral a) => [a] -> Int
countEvens2 = length . filter even

countEvens3 :: (Integral a) => [a] -> Int
countEvens3 = sum . map count
    where count x | even x = 1
                  | otherwise = 0

main = quickCheck ((\xs ->
        let count = countEvens xs
        in countEvens2 xs == count && count == countEvens3 xs) :: [Int] -> Bool)
