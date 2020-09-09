module Ex8 where

-- 1.

data Nat = Z | S Nat deriving Show

int2nat :: Int -> Nat
int2nat 0 = Z
int2nat n = S (int2nat (n - 1))

nat2int :: Nat -> Int
nat2int Z = 0
nat2int (S k) = 1 + nat2int k

add :: Nat -> Nat -> Nat
add Z n = n
add (S m) n = S (add m n)

mult :: Nat -> Nat -> Nat
mult Z _ = Z
mult (S m) n = add n (mult m n)

-- 2.

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l v r) = case compare x v of
                            EQ -> True
                            LT -> occurs x l
                            GT -> occurs x r

-- 3.

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show

leaves :: Tree1 a -> Int
leaves (Leaf1 _) = 1
leaves (Node1 l r) = leaves l + leaves r

balanced :: Tree1 a -> Bool
balanced (Leaf1 _) = True
balanced (Node1 l r) = balanced l && balanced r && abs (leaves l - leaves r) <= 1

-- 4.

balance :: [a] -> Tree1 a
balance [] = error "empty list"
balance [x] = Leaf1 x
balance xs = Node1 (balance l) (balance r)
  where
    (l, r) = (take n xs, drop n xs)
    n = length xs `div` 2

-- 5.

data Expr = Val Int | Add Expr Expr deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n 
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 6.

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- 7.

  {-

     instance Eq a => Eq (Maybe a) where
        Nothing == Nothing = True
        Just x == Just y = x == y
        _ == _ = False


    instance Eq a => Eq [a] where
        [] == [] = True
        (x : xs) == (y : ys) = x == y && xs == ys

     -}

-- 8. See TautChecker.hs

-- 9. See AbsMachine.hs