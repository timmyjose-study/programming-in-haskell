module CaesarCipher where

import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (n + ord 'a')

shift :: Int -> Char -> Char
shift k c
  | isLower c = int2let ((let2int c + k) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode k cs = [shift k c | c <- cs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent m n = (fromIntegral m / fromIntegral n) / 100.0

lowers :: String -> Int
lowers cs = length [c | c <- cs, isLower c]

count :: Char -> String -> Int
count c cs = length [c' | c' <- cs, c' == c]

freqs :: String -> [Float]
freqs cs = [percent (count c cs) n | c <- ['a' .. 'z']]
  where
    n = lowers cs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [p | (x', p) <- zip xs [0 ..], x' == x]

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs
