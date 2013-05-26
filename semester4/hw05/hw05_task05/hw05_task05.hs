module Main where

import Control.Monad

localMax :: (Ord a) => [a] -> Maybe a
localMax (pre:rest@(x:post:xs)) = getLocalMaxIfAny `mplus` localMax rest
    where getLocalMaxIfAny | x > pre && x > post = Just x
                           | otherwise           = Nothing
localMax _ = Nothing
