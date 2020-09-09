module AbstractMachine where

data Expr = Val Int
          | Add Expr Expr
          deriving Show

--value :: Expr -> Int
--value (Val n) = n
--value (Add x y) = value x + value y

e1 :: Expr
e1 = Add (Add (Val 2) (Val 3)) (Val 4)

-- abstract machine using control stacks

data Op = EVAL Expr | ADD Int deriving Show

type Cont = [Op]

-- now evaluation takes place in the context of the control stack, and this control stack defines the exact semantics
-- of evaluation

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c) -- so y is avaluated after x

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD m : c) n = exec c (m + n)

value :: Expr -> Int
value e = eval e []