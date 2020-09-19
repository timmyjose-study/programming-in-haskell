module Expr

%default total

data Expr = Val Int
          | Add Expr Expr

Show Expr where
  show (Val n) = show n
  show (Add x y) = show x ++ "+" ++ show y
          
Stack : Type
Stack = List Int

data Op = PUSH Int
        | ADD 

Code : Type
Code = List Op

exec : Code -> Stack -> Stack
exec [] s = s
exec (PUSH n :: c) s = exec c (n :: s)
exec (ADD :: c) s = case s of 
                         m :: n :: s => exec c (n + m :: s)
                         _ => []

comp : Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

eval : Expr -> Maybe Int
eval e = case exec (comp e) [] of
              [v] => Just v
              _ => Nothing