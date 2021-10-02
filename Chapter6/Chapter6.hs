{-# LANGUAGE TemplateHaskell #-}

module Chapter6 where

import Test.QuickCheck

fac :: Int -> Int
fac n = product [1 .. n]

fac' :: Int -> Int
fac' 0 = 1
fac' n = n * fac' (n - 1)

multOp :: Int -> Int -> Int
multOp _ 0 = 0
multOp m n = m + multOp m (n - 1)

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (n : ns) = n * myProduct ns

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x : xs) ys = x : myAppend xs ys

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop n xs
  | n < 0 = xs
  | otherwise = myDrop (n - 1) (tail xs)

prop_myDrop :: Eq a => Int -> [a] -> Bool
prop_myDrop n xs = myDrop n xs == drop n xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

myEven :: Int -> Bool
myEven 0 = True
myEven n = myOdd (n - 1)

myOdd :: Int -> Bool
myOdd 0 = False
myOdd n = myEven (n - 1)

evens :: [a] -> [a]
evens [] = []
evens (x : xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_ : xs) = evens xs

myInit :: [a] -> [a]
myInit [] = []
myInit [_] = []
myInit (x : xs) = x : myInit xs

return []

main = $forAllProperties (quickCheckWithResult stdArgs {maxSuccess = 1000})