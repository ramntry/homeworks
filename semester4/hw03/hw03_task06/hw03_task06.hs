module Main where

import Data.Maybe

checkBrackets xs = (start . filter (`elem` "(){}[]<>")) xs == Just ""
    where terminal sym (x:xs) | x == sym = Just xs
          terminal _ _ = Nothing

          start ('(':xs) = start xs >>= terminal ')' >>= start
          start ('{':xs) = start xs >>= terminal '}' >>= start
          start ('[':xs) = start xs >>= terminal ']' >>= start
          start ('<':xs) = start xs >>= terminal '>' >>= start
          start xs = Just xs
