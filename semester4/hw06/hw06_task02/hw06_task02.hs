module Main where

import Data.Monoid
import Data.List
import Control.Applicative

data Monomial a = Monomial { coeff :: a
                           , deg   :: Int
                           }
    deriving (Eq)

instance (Ord a) => Ord (Monomial a) where
    compare (Monomial c1 d1) (Monomial c2 d2) = compare d1 d2 `mappend` compare c1 c2

instance (Num a) => Monoid (Monomial a) where
    mempty = Monomial { coeff = 1, deg = 0 }
    (Monomial c1 d1) `mappend` (Monomial c2 d2) = Monomial (c1 * c2) (d1 + d2)

instance (Eq a, Show a, Num a) => Show (Monomial a) where
    show (Monomial 0 _) = "0"
    show (Monomial c d) = (if c == 1 && d /= 0 then "" else show c) ++ showX d
        where showX 0 = ""
              showX 1 = "x"
              showX p = "x^" ++ show p

newtype Polynomial a = Polynomial { getMonomials :: [Monomial a] }

instance (Eq a, Show a, Num a) => Show (Polynomial a) where
    show (Polynomial []) = "0"
    show (Polynomial ms) = (intercalate " + " . map show) ms

normalize :: (Num a, Ord a) => Polynomial a -> Polynomial a
normalize = Polynomial . map addCoeffs . groupBy isSimilar . reverse . sort . filter (\m -> coeff m /= 0) . getMonomials
    where addCoeffs = foldr1 step
          step (Monomial c1 d) (Monomial c2 _) = Monomial (c1 + c2) d
          isSimilar (Monomial _ d1) (Monomial _ d2) = d1 == d2

infixl 6 |+
(|+) :: (Num a, Ord a) => Polynomial a -> Polynomial a -> Polynomial a
p1 |+ p2 = (normalize . Polynomial) $ getMonomials p1 ++ getMonomials p2

infixl 7 |*
(|*) :: (Num a, Ord a) => Polynomial a -> Polynomial a -> Polynomial a
p1 |* p2 = (normalize . Polynomial) $ mappend <$> getMonomials p1 <*> getMonomials p2

main = do
    let p =  Polynomial [Monomial 1 0, Monomial (-2) 4, Monomial 0 4, Monomial 3 1]
    putStrLn $ "if p = " ++ (show . normalize) p ++ " then p^2 + p = " ++ show (p |* p |+ p)
