module Main where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

maxAdjacentSumIndex :: (Integral a) => [a] -> Int
maxAdjacentSumIndex xs = fromJust $ elemIndex (maximum zipped) zipped
    where zipped = zipWith (+) (0 : xs) (xs ++ [0])
