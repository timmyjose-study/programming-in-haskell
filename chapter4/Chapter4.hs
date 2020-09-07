{- Definining Functions -}

module Chapter4 where

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

recip' :: Fractional a => a -> a
recip' n = 1 / n

abs' :: Int -> Int
abs' n = if n < 0 then -n else n

signum' :: Int -> Int
signum' n = if n < 0 then -1 else 
              if n == 0 then 0 else 1

abs'' :: Int -> Int
abs'' n | n < 0 = -n
        | otherwise = n

signum'' :: Int -> Int
signum'' n | n < 0 = -1
           | n == 0 = 0
           | otherwise = 1

odds :: Int -> [Int]
odds n = map (\x -> 2 * x + 1) [0 .. n - 1]