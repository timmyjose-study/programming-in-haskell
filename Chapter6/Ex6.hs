{-# LANGUAGE TemplateHaskell #-}

module Ex6 where

import Test.QuickCheck

fac :: Int -> Int
fac 0 = 1
fac n
  | n < 0 = 1
  | otherwise = n * fac (n - 1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

pow :: Int -> Int -> Int
pow _ 0 = 1
pow m n = m * pow m (n - 1)

euclid :: Int -> Int -> Int
euclid a 0 = a
euclid a b = euclid b (a `mod` b)

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (b : bs) = b && myAnd bs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat [[]] = []
myConcat (xs : xss) = xs ++ myConcat xss

prop_myConcat :: Eq a => [[a]] -> Bool
prop_myConcat xss = myConcat xss == concat xss

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x
  | n < 0 = []
  | otherwise = x : myReplicate (n - 1) x

prop_myReplicate :: Eq a => Int -> a -> Bool
prop_myReplicate n x = myReplicate n x == replicate n x

nth :: Int -> [a] -> a
nth 0 (x : xs) = x
nth n (_ : xs) = nth (n - 1) xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y : ys)
  | x == y = True
  | otherwise = myElem x ys

prop_myElem :: Eq a => a -> [a] -> Bool
prop_myElem x xs = myElem x xs == elem x xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

halve :: [a] -> ([a], [a])
halve xs = (take len xs, drop len xs)
  where
    len = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    (left, right) = halve xs

mySum :: Num a => [a] -> a
mySum = foldl (+) 0

prop_mySum :: (Eq a, Num a) => [a] -> Bool
prop_mySum ns = mySum ns == sum ns

myTake :: Int -> [a] -> [a]
myTake 0 xs = []
myTake _ [] = []
myTake n (x : xs)
  | n < 0 = []
  | otherwise = x : myTake (n - 1) xs

prop_myTake :: Eq a => Int -> [a] -> Bool
prop_myTake n xs = myTake n xs == take n xs

myLast :: [a] -> a
myLast (x : []) = x
myLast (x : xs) = myLast xs

return []

main = $forAllProperties (quickCheckWithResult stdArgs {maxSuccess = 1000})
