module Main where

import Test.QuickCheck
import Text.Printf (printf)
import Data.List (elemIndex)

customElemIndex :: Eq a => a -> [a] -> Maybe Int
customElemIndex item l = helper 0 l
    where helper _ []     = Nothing
          helper i (x:xs) | x == item = Just i
                          | otherwise = helper (i + 1) xs


prop_correctness item l = customElemIndex item l == elemIndex item l

tests :: [(String, IO ())]
tests = [ ("Char in String", quickCheck (prop_correctness :: Char -> String -> Bool))
        , ("Ordering in [Ordering]", quickCheck (prop_correctness :: Ordering -> [Ordering] -> Bool))
        ]

main :: IO ()
main = mapM_ (\(name, test) -> printf "%-35s: " name >> test) tests
