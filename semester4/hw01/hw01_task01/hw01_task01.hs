module Main where

import System.IO

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = factorial (n - 1) * n

main = do
    hSetBuffering stdout NoBuffering
    putStr "Factorial\nn = "
    userInput <- getLine
    putStrLn $ show $ factorial (read userInput :: Integer)
