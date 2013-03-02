module Main where

import Test.QuickCheck
import Text.Printf (printf)


powersOfTwo :: [Integer]
powersOfTwo = 1 : map (* 2) powersOfTwo


takePowersOfTwo :: Int -> [Integer]
takePowersOfTwo = (flip take) powersOfTwo

referenceTakePowersOfTwo :: Int -> [Integer]
referenceTakePowersOfTwo n | n < 0     = error "n must be nonnegative"
                           | otherwise = helper 1 n
    where helper _            0       = []
          helper currentPower counter = currentPower : helper (currentPower * 2) (counter - 1)


prop_length :: Int -> Property
prop_length n = n >= 0 ==> length (takePowersOfTwo n) == n

prop_firstPowerIsOne :: Int -> Property
prop_firstPowerIsOne n = n > 0 ==> head (takePowersOfTwo n) == 1

prop_values :: Int -> Property
prop_values n = n > 0 ==> all (\(x, y) -> x == y * 2) $ zip (tail powers) powers
    where powers = takePowersOfTwo n

prop_identityWithReference :: Int -> Property
prop_identityWithReference n = n >= 0 ==> takePowersOfTwo n == referenceTakePowersOfTwo n

tests :: [(String, IO ())]
tests = [ ("length", quickCheckWith (stdArgs { maxSize = 25 }) prop_length)
        , ("first power is one", quickCheck prop_firstPowerIsOne)
        , ("values", quickCheckWith (stdArgs { maxSize = 25 }) prop_values)
        , ("identity with reference", quickCheckWith (stdArgs { maxSize = 25 }) prop_identityWithReference)
        ]

main :: IO ()
main = mapM_ (\(name, test) -> printf "%-35s: " name >> test) tests
