{-# LANGUAGE TemplateHaskell #-}

{- Recursive Functions -}

module Chapter6 where

import Test.QuickCheck

fac :: Int -> Int
fac 0 = 1
fac n | n < 0 = 1
      | otherwise = n * fac (n - 1)

mult :: Int -> Int -> Int
mult m 0 = 0
mult m n = m + mult m (n - 1)

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (n : ns) = n * myProduct ns

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys) | x <= y = x : y : ys
                  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x : xs) | n <= 0 = []
                  | otherwise = x : myTake (n - 1) xs

prop_myTake :: Eq a => Int -> [a] -> Bool
prop_myTake n xs = myTake n xs == take n xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (x : xs) | n <= 0 = x : xs
                  | otherwise = myDrop (n - 1) xs

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
myInit [] = error "Empty list"
myInit [x] = []
myInit (x : xs) = x : myInit xs

return []
main = $forAllProperties (quickCheckWithResult (stdArgs { maxSuccess = 100 }))