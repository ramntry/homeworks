module Main where

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

main :: IO ()
main = do
    putStrLn "Enter a string:"
    input <- getLine
    putStrLn $ if isPalindrome input then "is palindrome" else "isn't palindrome"
