slowFibonacci :: Integral a => a -> a
slowFibonacci 0 = 0
slowFibonacci 1 = 1
slowFibonacci n = slowFibonacci (n - 1) + slowFibonacci (n - 2)

fastFibonacci :: Integral a => a -> a
fastFibonacci = fastFibonacci' 1 0
                where fastFibonacci' current _           1 = current
                      fastFibonacci' current predecessor n = fastFibonacci' (current + predecessor) current (n - 1)

fibonacciNumbers :: [Integer]
fibonacciNumbers = 1 : 1 : zipWith (+) fibonacciNumbers (tail fibonacciNumbers)
