module Ex5 where

sumSqr100 :: Int
sumSqr100 = sum [n ^ 2 | n <- [1 .. 100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

square :: Int -> [(Int, Int)]
square n = filter (\(x, y) -> x /= y) $ grid n n

myReplicate :: Int -> a -> [a]
myReplicate n x = [x | _ <- [1 .. n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [x .. n], z <- [x .. n], x ^ 2 + y ^ 2 == z ^ 2]

factors :: Int -> [Int]
factors n = [f | f <- [1 .. n], n `mod` f == 0]

perfects :: Int -> [Int]
perfects n = [p | p <- [1 .. n], sum (init (factors p)) == p]

nestedComprehension :: [(Int, Int)]
nestedComprehension = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

find :: Eq k => k -> [(k, v)] -> [v]
find k t = [p | (k', p) <- t, k' == k]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x $ zip xs [0 ..]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
