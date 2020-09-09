{- Declaring Types and Classes -}

module Chapter8 where

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v' | (k', v') <- t, k' == k]

type Pos = (Int, Int)

data Move = North | East | South | West
  deriving (Eq, Ord, Read, Show)

move :: Move -> Pos -> Pos
move North (x, y) = (x + 1, y)
move East (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move West (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m : ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

data Shape = Circle Float | Rectangle Float Float
  deriving Show

area :: Shape -> Float
area (Circle radius) = pi * radius ^ 2
area (Rectangle length breadth) = length * breadth

square :: Float -> Shape
square side = Rectangle side side

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x : _) = Just x

data Nat = Z | S Nat deriving Show

nat2int :: Nat -> Int
nat2int Z = 0
nat2int (S k) = 1 + nat2int k

int2nat :: Int -> Nat
int2nat 0 = Z
int2nat n = S (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Z n = n
add (S m) n = S (add m n)

mult :: Nat -> Nat -> Nat
mult Z m = Z
mult (S m) n = add n (mult m n)

data List a = Nil | Cons a (List a) deriving Show

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 3)) 5
         (Node (Leaf 6) 7 (Leaf 9))

--occurs :: Eq a => a -> Tree a -> Bool
--occurs x (Leaf y) = x == y
--occurs x (Node l v r) = x == v || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l v r) = flatten l ++ [v] ++ flatten r

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l v r) | x == v = True
                      | x < v = occurs x l
                      | otherwise = occurs x r