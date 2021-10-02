module Ex4 where

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

third :: [a] -> a
third = head . tail . tail

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_ : _ : x : _) = x

safetail :: [a] -> [a]
safetail xs =
  if null xs
    then []
    else tail xs

safetail' :: [a] -> [a]
safetail' xs
  | null xs = []
  | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_ : xs) = xs

luhnDouble :: Int -> Int
luhnDouble n = if val > 9 then (val - 9) else val
  where
    val = 2 * n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = total `mod` 10 == 0
  where
    total = d + luhnDouble c + b + luhnDouble a
