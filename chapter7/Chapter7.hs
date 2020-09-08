{-# LANGUAGE TemplateHaskell #-}

{- Higher-order Functions -}

module Chapter7 where
  
import Test.QuickCheck

add :: Int -> Int -> Int
add = \x -> \y -> x + y

twice :: (a -> a) -> a -> a
twice f x = f (f x)

myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = [f x | x <- xs]

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

prop_myMap :: Eq b => Fun a b -> [a] -> Bool
prop_myMap (Fn f) xs = myMap f xs == map f xs

prop_myMap' :: Eq b => Fun a b -> [a] -> Bool
prop_myMap' (Fn f) xs = myMap' f xs == map f xs

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' _ [] = []
myFilter' p (x : xs) | p x = x : myFilter' p xs
                     | otherwise = myFilter' p xs

prop_myFilter' :: Eq a => Fun a Bool -> [a] -> Bool
prop_myFilter' (Fn p) xs = myFilter' p xs == filter p xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = [x | x <-  xs, p x]

prop_myFilter :: Eq a => Fun a Bool -> [a] -> Bool
prop_myFilter (Fn p) xs = myFilter p xs == filter p xs

sumsqreven :: [Int] -> Int
sumsqreven = sum . map (^2) . filter even

  {-

     Pattern:

     f [] = v
     f v (x : xs) = x # f xs

     -}

length1 :: [a] -> Int
length1 [] = 0
length1 (_ : xs) = 1 + length1 xs

sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (n : ns) = n + sum1 ns

product1 :: Num a => [a] -> a
product1 [] = 1
product1 (n : ns) = n * product1 ns

or1 :: [Bool] -> Bool
or1 [] = False
or1 (b : bs) = b || or1 bs

and1 :: [Bool] -> Bool
and1 [] = True
and1 (b : bs) = b && and1 bs

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x : xs) = reverse xs ++ [x]

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ v [] = v
myFoldr f v (x : xs) = f x (myFoldr f v xs)

length2 :: [a] -> Int
length2 = myFoldr (\_ acc -> 1 + acc) 0

prop_length1 :: [a] -> Bool
prop_length1 xs = length1 xs == length2 xs

sum2 :: Num a => [a] -> a
sum2 = myFoldr (+) 0

prop_sum1 :: (Eq a, Num a) => [a] -> Bool
prop_sum1 ns = sum1 ns == sum2 ns

product2 :: Num a => [a] -> a
product2 = myFoldr (*) 1

prop_product1 :: (Eq a, Num a) => [a] -> Bool
prop_product1 ns = product1 ns == product2 ns

or2 :: [Bool] -> Bool
or2 = myFoldr (||) False

prop_or1 :: [Bool] -> Bool
prop_or1 bs = or1 bs == or2 bs

and2 :: [Bool] -> Bool
and2 = myFoldr (&&) True

prop_and1 :: [Bool] -> Bool
prop_and1 bs = and1 bs == and2 bs

reverse2 :: [a] -> [a]
reverse2 = myFoldr (\x acc -> acc ++ [x]) []

prop_revers1 :: Eq a => [a] -> Bool
prop_revers1 xs = reverse1 xs == reverse2 xs

  {-
     Pattern:

     f v [] = v
     f v (x : xs) = f (v # x) xs

     -}

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ v [] = v
myFoldl f v (x : xs) = myFoldl f (f v x) xs

length3 :: [a] -> Int
length3 = myFoldl (\acc _ -> acc + 1) 0

sum3 :: Num a => [a] -> a
sum3 = myFoldl (+) 0

product3 :: Num a => [a] -> a
product3 = myFoldl (*) 1

or3 :: [Bool] -> Bool
or3 = myFoldl (||) False

and3 :: [Bool] -> Bool
and3 = myFoldl (&&) True

reverse3 :: [a] -> [a]
reverse3 = myFoldl (\acc x -> x : acc) []

prop_length2 :: [a] -> Bool
prop_length2 xs = length2 xs == length3 xs

prop_sum2 :: (Eq a, Num a) => [a] -> Bool
prop_sum2 ns = sum2 ns == sum3 ns

prop_product2 :: (Eq a, Num a) => [a] -> Bool
prop_product2 ns = product2 ns == product3 ns

prop_or2 :: [Bool] -> Bool
prop_or2 bs = or2 bs == or3 bs

prop_and2 :: [Bool] -> Bool
prop_and2 bs = and2 bs == and3 bs

prop_reverse2 :: Eq a => [a] -> Bool
prop_reverse2 xs = reverse2 xs == reverse3 xs

return []
main = $forAllProperties (quickCheckWithResult (stdArgs { maxSuccess = 1000 }))