module Chapter4 where

myEven :: Int -> Bool
myEven n = n `mod` 2 == 0

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n xs = (take n xs, drop n xs)

myRecip :: Fractional a => a -> a
myRecip n = 1 / n

myAbs :: Int -> Int
myAbs n = if n < 0 then -n else n

mySignum :: Int -> Int
mySignum n =
  if n < 0
    then -1
    else
      if n == 0
        then 0
        else 1

myAbs' :: Int -> Int
myAbs' n
  | n < 0 = -n
  | otherwise = n

mySignum' :: Int -> Int
mySignum' n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

myNot :: Bool -> Bool
myNot False = True
myNot True = False

myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

odds :: Int -> [Int]
odds n = filter (\x -> x `mod` 2 == 1) [1 .. n]

mySum :: Num a => [a] -> a
mySum = foldl (+) 0

myProduct :: Num a => [a] -> a
myProduct = foldl (*) 1