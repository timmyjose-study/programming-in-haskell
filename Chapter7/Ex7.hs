{-# LANGUAGE TemplateHaskell #-}

module Ex7 where

import Test.QuickCheck

filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f xs = [f x | x <- xs, p x]

filterMap' :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap' p f = map f . filter p

myAll :: (a -> Bool) -> [a] -> Bool
myAll p = foldl (\acc x -> acc && p x) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldl (\acc x -> acc || p x) False

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x : xs)
  | p x = x : myTakeWhile p xs
  | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile p (x : xs)
  | p x = myDropWhile p xs
  | otherwise = (x : xs)

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x acc -> if p x then x : acc else acc) []

dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

addCurried :: Int -> Int -> Int
addCurried x y = x + y

addUncurried :: (Int, Int) -> Int
addUncurried (x, y) = x + y

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f = \x -> \y -> f (x, y)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f = \(a, b) -> f a b

addCurried' :: Int -> Int -> Int
addCurried' = myCurry addUncurried

addUncurried' :: (Int, Int) -> Int
addUncurried' = myUncurry addCurried

prop_curried :: Int -> Int -> Bool
prop_curried x y = addCurried x y == addCurried' x y

prop_uncurried :: (Int, Int) -> Bool
prop_uncurried (x, y) = addUncurried (x, y) == addUncurried' (x, y)

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin n = unfold (== 0) (`mod` 2) (`div` 2) n

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ [x] = [f x]
altMap f g (x : y : zs) = f x : g y : altMap f g zs

luhnDouble :: Int -> Int
luhnDouble n = if val > 9 then val - 9 else val
  where
    val = n * 2

luhn :: [Int] -> Bool
luhn ds = total `mod` 10 == 0
  where
    total = sum . altMap id luhnDouble . reverse $ ds

return []

main = $forAllProperties (quickCheckWithResult stdArgs {maxSuccess = 1000})