module Ex5 where

-- 1.

sumSqrTillHundred :: Int
sumSqrTillHundred = sum [n^2 | n <- [1..100]]

-- 2.

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 3.

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4.

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- 5.

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [x..n], z <- [x..n], x^2 + y^2 == z^2]

-- 6.

perfects :: Int -> [Int]
perfects n = [p | p <- [1..n], sum (factors p) == p]
  where
    factors n = [d | d <- [1..n-1], n `mod` d == 0]

-- 7.

gen :: [(Int, Int)]
gen = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

-- 8.

find :: Eq k => k -> [(k, v)] -> [v]
find k t = [v | (k', v) <- t, k' == k]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- 9.

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <-  zip xs ys]