{- The Countdown problem -}

module Chapter9 where

data Op = Add | Sub | Mul | Div 

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _  = True
valid Sub x y = x > y
valid Mul _ _  = True
valid Div x y = y /= 0 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | Apply Op Expr Expr

instance Show Expr where
  show (Val n) = show n 
  show (Apply o x y) = brak x ++ show o ++ brak y
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (Apply o l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (Apply o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x:) yss
  where
    yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

e :: Expr
e = Apply Mul (Apply Add (Val 1) (Val 50)) (Apply Sub (Val 25) (Val 10))

ns :: [Int]
ns = [1, 3, 7, 10, 25, 50]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = 
  elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x  : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [Apply o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = 
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]