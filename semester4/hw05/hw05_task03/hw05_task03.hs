module Main where

import Data.List
import Test.QuickCheck

wordsInSimpleABC :: Int -> [[Int]]
wordsInSimpleABC = sequence . flip replicate [1, 2, 3]

prop_unique :: Int -> Bool
prop_unique n = all ((== 1) . length) $ group $ sort $ wordsInSimpleABC (abs n)

prop_size :: Int -> Bool
prop_size n = (length . wordsInSimpleABC) (abs n) == 3 ^ (abs n)

main = quickCheckWith stdArgs { maxSize = 7 } prop_unique >>
       quickCheckWith stdArgs { maxSize = 7 } prop_size
