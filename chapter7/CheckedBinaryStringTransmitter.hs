module CheckedBinaryStringTransmitter where

import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

calculateParity :: [Bit] -> Bit
calculateParity bits = bitcount `mod` 2
  where
    bitcount = sum $ filter (== 1) bits

addParity :: [Bit] -> [Bit]
addParity bits = bits ++ [calculateParity bits]

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity bits = if calculateParity bs == last bits
                      then bs
                      else error "Parity mismatch"
  where
    bs = init bits

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chop9 

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

badTransmit :: String -> String
badTransmit = decode . faultyChannel . encode