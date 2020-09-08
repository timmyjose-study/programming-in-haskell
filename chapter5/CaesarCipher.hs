module CaesarCipher where

import           Data.Char

let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | isUpper c = ord c - ord 'A' + 26 -- make it a contiguous array of 52 elements

int2let :: Int -> Char
int2let n | n < 26 = chr (n + ord 'a')
          | otherwise = chr (n - 26 + ord 'A')

shift :: Int -> Char -> Char
shift n c | isLetter c = int2let ((let2int c + n) `mod` 52)
          | otherwise = c

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1,8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent m n = (fromIntegral m / fromIntegral n) * 100.0

lowers :: String -> Int
lowers cs = length [c | c <- cs, isLower c]

count :: Char -> String -> Int
count c cs = length [1 | c' <- cs, c' == c]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [p | (x', p) <- zip xs [0..], x' == x]

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
  where
    n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(o - e)^2 / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack cs = encode (-factor) cs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..51]]
    table' = freqs cs
