module Main where

func :: (b -> Bool) -> (a -> b) -> [a] -> [b]
func = flip (.) map . (.) . filter
