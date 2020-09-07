{- Types and Classes -}

module Chapter3 where

add :: (Int, Int) -> Int
add (x, y) = x + y

zeroto :: Int -> [Int]
zeroto n = [1 .. n]

mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z