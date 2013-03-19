module Main where

pairProduct :: Int -> [Int]
pairProduct n = let nums = [1..n] in nums >>= \x -> nums >>= return . (* x)
