module AbsMachine where

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving Show

data Op = EVALADD Expr
        | EVALSUB Expr
        | EVALMUL Expr
        | EVALDIV Expr
        | ADD Int
        | SUB Int
        | MUL Int
        | DIV Int
        deriving Show

type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVALADD y : c)
eval (Sub x y) c = eval x (EVALSUB y : c)
eval (Mul x y) c = eval x (EVALMUL y : c)
eval (Div x y) c = eval x (EVALDIV y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALADD y : c) n = eval y (ADD n : c)
exec (EVALSUB y : c) n = eval y (SUB n : c)
exec (EVALMUL y : c) n = eval y (MUL n : c)
exec (EVALDIV y : c) n = eval y (DIV n : c)
exec (ADD n : c) m = exec c (n + m)
exec (SUB n : c) m = exec c (n - m)
exec (MUL n : c) m = exec c (n * m)
exec (DIV n : c) m = exec c (if m == 0 then 0 else n `div` m)

value :: Expr -> Int
value e = eval e []

-- 1 + 2 * 3
e1 :: Expr
e1 = Add (Val 1) (Mul (Val 2) (Val 3))

-- (1 + 2) * (3 + 4)
e2 :: Expr
e2 = Mul (Add (Val 1) (Val 2)) (Add (Val 3) (Val 4))

-- 10 / (1 + 4)
e3 :: Expr
e3 = Div (Val 10) (Add (Val 1) (Val 4))

