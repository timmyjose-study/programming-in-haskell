module Hangman where

import System.IO (hSetEcho, stdin)

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Now try to guess it!"
             play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' 
                 then do putChar x
                         return []
                 else do putChar '-'
                         xs <- sgetLine
                         return (x : xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> IO ()
play secret = do putStr "? "
                 guess <- getLine
                 if guess == secret 
                    then putStrLn "You guessed it!"
                    else do putStrLn (match guess secret)
                            play secret

match :: String -> String -> String
match guess secret = [if elem x guess then x else '-' | x <- secret]
