module Chapter5 where

myConcat :: [[a]] -> [a]
myConcat xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [f | (f, _) <- ps]

myLength :: [a] -> Int
myLength xs = sum [1 | _ <- xs]

factors :: Int -> [Int]
factors n = [f | f <- [1 .. n], n `mod` f == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [p | p <- [1 .. n], prime p]

find :: Eq k => k -> [(k, v)] -> [v]
find k t = [v' | (k', v') <- t, k' == k]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [p | (x', p) <- zip xs [0 ..], x == x']

lowers :: String -> Int
lowers cs = length [c | c <- cs, c >= 'a' && c <= 'z']

count :: Char -> String -> Int
count c cs = length [c' | c' <- cs, c' == c]