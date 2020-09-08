{-# LANGUAGE TemplateHaskell #-}

module Ex7 where

import           Data.Char
import           Test.QuickCheck

-- 1.

filterMap :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filterMap f p = map f . filter p

filterMap' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filterMap' f p  xs = [f x | x <- xs, p x]

prop_filterMap :: Eq b => Fun a b -> Fun a Bool -> [a] -> Bool
prop_filterMap (Fn f) (Fn p) xs = filterMap f p xs == filterMap' f p xs

-- 2.

myAll :: (a -> Bool) -> [a] -> Bool
myAll p = foldl (\acc b -> acc && p b) True

prop_myAll :: Fun a Bool -> [a] -> Bool
prop_myAll (Fn p) xs = myAll p xs == all p xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldl (\acc x -> acc || p x) False

prop_myAny :: Fun a Bool -> [a] -> Bool
prop_myAny (Fn p) xs = myAny p xs == any p xs

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x : xs) | p x = x : myTakeWhile p xs
                       | otherwise = []

prop_myTakeWhile :: Eq a => Fun a Bool -> [a] -> Bool
prop_myTakeWhile (Fn p) xs = myTakeWhile p xs == takeWhile p xs

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile p (x : xs) | p x = myDropWhile p xs
                       | otherwise = (x : xs)

prop_myDropWhile :: Eq a => Fun a Bool -> [a] -> Bool
prop_myDropWhile (Fn p) xs = myDropWhile p xs == dropWhile p xs

-- 3.

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

prop_myMap :: Eq b => Fun a b -> [a] -> Bool
prop_myMap (Fn f) xs = myMap f xs == map f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x acc -> if p x then x : acc else acc) []

prop_myFilter :: Eq a => Fun a Bool -> [a] -> Bool
prop_myFilter (Fn p) xs = myFilter p xs == filter p xs

-- 4.

dec2int :: [Int] -> Int
dec2int = foldl (\acc d -> acc * 10 + d) 0

-- 5.

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f = \a -> \b -> f (a, b)

prop_myCurry :: Eq c => Fun (a, b) c -> a -> b -> Bool
prop_myCurry (Fn f) a b = (myCurry f) a b == (curry f) a b

myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f = \(a, b) -> f a b

--prop_myUncurry :: Eq c => Fun (a, b) c -> (a, b) -> Bool
--prop_myUncurry (Fn f) (a, b) = (myUncurry f) (a, b) == (uncurry f) (a, b)

-- 6.

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

mapU :: (a -> b) -> [a] -> [b]
mapU f = unfold null (f . head) tail

iterateU :: (a -> a) -> a -> [a]
iterateU f = unfold (const False) id f

-- 7 & 8. See CheckedBinaryStringTransmitter.hs

-- 9.

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _  [] = []
altMap f _ [x] = [f x]
altMap f g (x : y : zs) = f x : g y : altMap f g zs

-- 10.

luhnDouble :: Int -> Int
luhnDouble d = if s > 9 then s - 9 else s
  where
    s = d * 2

luhn :: [Int] -> Bool
luhn ns = total `mod` 10 == 0
  where
    total = sum . altMap luhnDouble id $ ns

return []
main = $forAllProperties (quickCheckWithResult (stdArgs { maxSuccess = 1000 }))