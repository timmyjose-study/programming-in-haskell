module Chapter2 where

double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple = double . double

factorial :: Integral a => a -> a
factorial n = product [1 .. n]

average :: [Int] -> Int
average ns = sum ns `div` length ns