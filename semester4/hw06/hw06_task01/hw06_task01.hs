module Main where

import Data.Maybe

data Tree = Empty
          | Node Char Tree Tree
    deriving (Show, Eq)

parse' :: String -> Maybe (Tree, String)
parse' ('e':xs) = Just (Empty, xs)
parse' ('n':x:xs) = do
    (left, rem') <- parse' xs
    (right, rem) <- parse' rem'
    return (Node x left right, rem)
parse' _ = Nothing

parse :: String -> Maybe Tree
parse s = case parse' s of
    Just (t, "") -> Just t
    _            -> Nothing

gen :: Tree -> String
gen Empty = "e"
gen (Node x left right) = "n" ++ [x] ++ gen left ++ gen right

main = putStrLn $ "code: " ++ code ++ "\ntree: " ++ show tree ++ "\ncode again: " ++ codeAgain
    where code = "naenbee"
          tree = parse code
          codeAgain = gen $ fromJust tree
