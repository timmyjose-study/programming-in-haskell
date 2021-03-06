{-# LANGUAGE TemplateHaskell #-}

module Ex6 where
  
import Test.QuickCheck

-- 1.

fac :: Int -> Int
fac 0 = 1
fac n | n < 0 = 1
      | otherwise = n * fac (n - 1)

-- 2.

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3.

pow :: Int -> Int -> Int
pow _ 0 = 1
pow a b = a * pow a (b - 1)

-- 4.

euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a < b = euclid a (b - a)
           | otherwise = euclid (a - b) b

-- 6.

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (b : bs) = b && myAnd bs

prop_myAnd :: [Bool] -> Bool
prop_myAnd bs = myAnd bs == and bs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat [[x]] = [x]
myConcat (xs : xss) = xs ++ myConcat xss

prop_myConcat :: Eq a => [[a]] -> Bool
prop_myConcat xss = myConcat xss == concat xss

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x | n < 0 = []
                | otherwise = x : myReplicate (n - 1) x

prop_myReplicate :: Eq a => Int -> a -> Bool
prop_myReplicate n x = myReplicate n x == replicate n x

index :: [a] -> Int -> a
index  [] _ = error "index too large"
index (x : xs) n | n < 0 = error "negative index"
                 | n == 0 = x
                 | otherwise = index xs (n - 1)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y : ys) = x == y || myElem x ys

prop_myElem :: Eq a => a -> [a] -> Bool
prop_myElem x xs = myElem x xs == elem x xs

-- 7.

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) | x <= y = x : merge xs (y : ys)
                        | otherwise = y : merge (x : xs) ys

-- 8.

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    (left, right) = (take n xs, drop n xs)
    n = length xs `div` 2

-- 9.

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (n : ns) = n + mySum ns

prop_mySum :: (Eq a, Num a) => [a] -> Bool
prop_mySum ns = mySum ns == sum ns

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x : xs) | n < 0 = []
                  | otherwise = x : myTake (n - 1) xs

prop_myTake :: Eq a => Int -> [a] -> Bool
prop_myTake n xs = myTake n xs == take n xs

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

return []
main = $forAllProperties (quickCheckWithResult (stdArgs { maxSuccess = 1000 }))
