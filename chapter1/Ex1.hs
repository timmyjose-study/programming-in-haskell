module Ex1 where

-- 3.
myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (n : ns) = n * myProduct ns

-- 4.
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort bigger ++ [x] ++ qsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    bigger = [b | b <- xs, b > x]
