module Chapter17 where

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving Show

type Stack = [Int]

data Code = HALT
          | PUSH Int Code
          | ADD Code
          | SUB Code
          | MUL Code
          | DIV Code
          deriving Show

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m : n : s) = exec c (n + m : s)
exec (SUB c) (m : n : s) = exec c (n - m : s)
exec (MUL c) (m : n : s) = exec c (n * m : s)
exec (DIV c) (m : n : s) = exec c ((if m == 0 then 0 else n `div` m) : s)

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' (Sub x y) c = comp' x (comp' y (SUB c))
comp' (Mul x y) c = comp' x (comp' y (MUL c))
comp' (Div x y) c = comp' x (comp' y (DIV c))

eval :: Expr -> Int
eval e = case exec (comp e) [] of
           [v] -> v
           _ -> error "Invalid expression"

