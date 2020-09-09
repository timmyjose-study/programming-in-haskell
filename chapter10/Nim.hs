module Nim where

import Data.Char (isDigit, digitToInt)

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1..] board]
  where
    update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] = do putRow 1 a 
                              putRow 2 b 
                              putRow 3 c 
                              putRow 4 d 
                              putRow 5 e 

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x
                        then return (digitToInt x)
                        else do putStrLn "error: invalid digit"
                                getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player = 
  do newline
     putBoard board
     if finished board 
        then do newline
                putStr $ "Player " ++ show (next player) ++ " wins!\n"
        else do newline
                putStr $ "Player " ++ show player ++ "\n"
                row <- getDigit "Enter a row number: "
                stars <- getDigit "Enter number of stars to remove: "
                if valid board row stars
                   then play (move board row stars) (next player)
                   else do newline
                           putStrLn "error: Invalid move!"
                           play board player

nim :: IO ()
nim = play initial 1