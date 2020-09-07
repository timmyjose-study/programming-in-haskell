{- List Comprehensions -}

module Chapter5 where

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

factors :: Int -> [Int]
factors n = [d | d <- [1..n], n `mod` d == 0]

prime :: Int -> Bool
prime p = factors p == [1, p]

primes :: Int -> [Int]
primes n = [m | m <- [2..n], prime m]

find :: Eq k => k -> [(k, v)] -> [v]
find k t = [v | (k', v) <- t, k' == k]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (i, x') <- zip [0..] xs, x == x']

lowers :: String -> String
lowers xs = [c | c <- xs, c >= 'a' && c <= 'z']

count :: Char -> String -> Int
count c cs = length [c' | c' <- cs, c' == c]