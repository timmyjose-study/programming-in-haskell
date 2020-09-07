module Ex4 where

-- 1.

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

-- 2.

third :: [a] -> a
third (_ : _ : x : _) = x

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' = head . tail . tail

-- 3.

safetail :: [a] -> [a]
safetail xs | null xs = xs
            | otherwise = tail xs

safetail' :: [a] -> [a]
safetail' xs = if null xs then xs else tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_ : xs) = xs

-- 8.

luhnDouble :: Int -> Int
luhnDouble n = if v > 9 then v - 9 else v
  where
    v = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = total `mod` 10 == 0
  where
    total = luhnDouble a + b + luhnDouble c + d
