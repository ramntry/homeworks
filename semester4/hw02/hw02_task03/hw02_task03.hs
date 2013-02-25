module Main where

import Test.QuickCheck
import Data.Char (ord)

slowSumOfDigits :: Integer -> Int
slowSumOfDigits n | n < 0     = error "n must be nonnegative"
                  | otherwise = helper 0 n
    where helper acc 0 = acc
          helper acc n = helper (acc + fromIntegral (n `mod` 10)) (n `div` 10)

stupidSumOfDigits :: Integer -> Int
stupidSumOfDigits n | n < 0     = error "n must be nonnegative"
                    | otherwise = sum $ map (\c -> ord c - (ord '0')) $ show n

main = quickCheck (\n -> n >= 0 ==> slowSumOfDigits n == stupidSumOfDigits n)
