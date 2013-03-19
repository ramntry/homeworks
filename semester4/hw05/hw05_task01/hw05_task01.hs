module Main where

partitions :: Int -> [[Int]]
partitions n = let f1 xs xss = map (xs ++) xss
                   f2 k s xss = f1 (replicate k s) xss
                   f3 s n | n == 0 = [[]]
                          | n < s = []
                          | otherwise = concat $ map (\k -> f2 k s (f3 (s + 1) (n - k * s))) [0..(n `div` s)]
               in f3 1 n
