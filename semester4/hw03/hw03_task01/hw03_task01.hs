module Main where

import Test.QuickCheck (quickCheck)

referenceFunc :: (Num a) => a -> [a] -> [a]
referenceFunc x l = map (\y -> y * x) l

-- func x l = map (\y -> y * x) l
-- func x l = map (\y -> x * y) l       by commutativity of multiplication
-- func x l = map (\y -> (*) x y) l     by removing some syntactic sugar
-- func x l = map ((*) x) l             by eta conversion
-- func x   = map ((*) x)               by eta conversion again
-- func x   = map (x *)                 by returning the syntactic sugar

func :: (Num a) => a -> [a] -> [a]
func x = map (x *)

main = quickCheck ((\x l -> referenceFunc x l == func x l) :: Int -> [Int] -> Bool)
