module Ex1 where

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (n : ns) = n * myProduct ns

revQsort :: Ord a => [a] -> [a]
revQsort [] = []
revQsort [x] = [x]
revQsort (x : xs) = revQsort bigger ++ [x] ++ revQsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    bigger = [b | b <- xs, b > x]
