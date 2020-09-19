module Chapter16 where

data Expr = Val Int | Add Expr Expr deriving Show

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]

data Op = PUSH Int | ADD deriving Show

type Code = [Op]

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (n + m : s)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

run :: Expr -> Int
run e = case exec (comp e) [] of
          [] -> error "Invalid expression"
          [v] -> v

e1 :: Expr
e1 = Add (Add (Val 1) (Val 2)) (Val 3)