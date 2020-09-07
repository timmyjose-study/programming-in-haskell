module Ex2 where

-- 3.
n :: Int
n = a `div` length ns
  where
    a = 10
    ns = [1, 2, 3, 4, 5]

-- 4.

last' :: [a] -> a
last' = head . reverse . tail

last'' :: [a] -> a
last'' = head . init . reverse

-- 5.

init' :: [a] -> [a]
init' = reverse . tail . reverse

init'' :: [a] -> [a]
init'' xs = take n xs
  where
    n = length xs - 1