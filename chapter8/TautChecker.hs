module TautChecker where

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop
          deriving Show

-- a ^ ~a
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

-- (a ^ b) -> a
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- a -> (a ^ b)
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (a ^ (a -> b)) -> b
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- (a <-> b) v (b <-> a)
p5 :: Prop
p5 = Or (Equiv (Var 'A') (Var 'B')) (Equiv (Var 'B') (Var 'A'))

-- a v ~a
p6 :: Prop
p6 = Or (Var 'A') (Not (Var 'A'))

-- (a <-> b) ^ ~(a <-> b)
p7 :: Prop
p7 = And (Equiv (Var 'A') (Var 'B')) (Not (Equiv (Var 'A') (Var 'B')))

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v' | (k', v') <- t, k' == k]

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval s (Const b) = b
eval s (Not p) = not (eval s p)
eval s (Var c) = find c s
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where
    bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
