module Main where

data Expression = Variable
                | Constant Int
                | Plus Expression Expression
                | Prod Expression Expression
                | Pow Expression Int

instance Show Expression where
    show Variable = "x"
    show (Constant a) = show a
    show (Plus x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Prod x y) = show x ++ "*" ++ show y
    show (Pow x n) = show x ++ "^" ++ show n

var = Variable
cst x = Constant x

infixl 6 |+
infixl 6 |-
infixl 7 |*
infixl 7 |/
infixr 8 |^

x |+ y = Plus x y
x |* y = Prod x y
x |^ n = Pow x n
x |- y = x |+ (cst (-1) |* y)
x |/ y = x |* (y |^ (-1))

diff :: Expression -> Expression
diff (Constant _) = cst 0
diff Variable = cst 1
diff (Plus x y) = diff x |+ diff y
diff (Prod x y) = diff x |* y |+ diff y |* x
diff (Pow x n) = cst n |* x |^ (n - 1) |* diff x

simplify :: Expression -> Expression
simplify (Constant x) = cst x
simplify Variable = var

simplify (Prod x y) = case (simpleX, simpleY) of
    (Constant a, Constant b) -> cst (a * b)
    (Constant a, Prod (Constant b) sy) -> simplify $ cst (a * b) |* sy
    (sx, Constant a) -> simplify $ cst a |* sx
    (Constant 1, sy) -> sy
    (Constant 0, _) -> cst 0
    (sx, sy) -> sx |* sy
    where simpleX = simplify x
          simpleY = simplify y

simplify (Plus x y) = case (simpleX, simpleY) of
    (Constant a, Constant b) -> cst (a + b)
    (Constant a, Plus (Constant b) sy) -> simplify $ cst (a + b) |+ sy
    (sx, Constant a) -> simplify $ cst a |* sx
    (Constant 0, sy) -> sy
    (sx, sy) -> sx |+ sy
    where simpleX = simplify x
          simpleY = simplify y

simplify (Pow x 0) = cst 1
simplify (Pow x 1) = x
simplify (Pow x n) = x |^ n

d :: Expression -> Expression
d = simplify . diff

main = do
    let expr = (var |+ cst 1) |/ (var |+ cst 2)
    putStrLn $ "d/dx " ++ show expr ++ " = " ++ (show . d) expr
