module Chapter8 where

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v' | (k', v') <- t, k' == k]

type Pos = (Int, Int)

data Move = North | East | South | West
  deriving (Show)

move :: Move -> Pos -> Pos
move North (x, y) = (x + 1, y)
move East (x, y) = (x, y + 1)
move South (x, y) = (x - 1, y)
move West (x, y) = (x, y - 1)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m : ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev East = West
rev South = North
rev West = East

data Shape = Circle Float | Rect Float Float deriving (Show)

square :: Float -> Shape
square side = Rect side side

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect l b) = l * b

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x : _) = Just x

data Nat = Zero | Succ Nat
  deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add m Zero = m
add m (Succ n) = Succ (add m n)

data List a = Nil | Cons a (List a) deriving (Show)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- O(n)
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l v r) = x == v || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- O(log n)
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r)
  | x < y = occurs' x l
  | x == y = True
  | otherwise = occurs' x r

{-
   class Eq where
    (==_), (/=) :: a -> a -> Bool

    x /= y = not (x == y)

    class Eq a => Ord a where
      (<), (<=), (>), (>=) :: a -> a -> Bool
      min, max :: a -> a -> a

      min x y | x <= y = x
              | otherwise = y

      max x y | x <= y = y
              | otherwise = x

-}