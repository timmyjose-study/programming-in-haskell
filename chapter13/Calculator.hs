module Main where

import Control.Applicative
import Data.Char
import System.IO

import qualified Parsick as P

  {-
     expr ::= term ( + expr | - expr | epsilon )
     term ::= factor ( * term | / term | epsilon )
     factor ::= ( expr ) | int
     int = ... | -1 | 0 | 1 | ...

     -}

-- arithmetic expressions parsers

expr :: P.Parser Int
expr = do t <- term
          do P.symbol "+"
             e <- expr
             return (t + e)
             <|> do P.symbol "-"
                    e <- expr
                    return (t - e)
             <|> return t

term :: P.Parser Int
term = do f <- factor
          do P.symbol "*"
             t <- term
             return (f * t)
             <|> do P.symbol "/"
                    t <- term
                    return (f `div` t)
             <|> return f
            
factor :: P.Parser Int
factor = do P.symbol "("
            e <- expr
            P.symbol ")"
            return e
            <|> P.integer

-- UI utilities

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

writeat :: Pos -> String -> IO ()
writeat pos message = do goto pos
                         putStr message

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

-- calculator logic

buttons :: String
buttons = standard ++ extra
  where
    standard = "qdc=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"

newline :: IO ()
newline = putStrLn ""

showbox :: IO ()
showbox = sequence_ $ [cls] ++ [writeat (1, y) b | (y, b) <- zip [1..] box] ++ [newline]

display :: String -> IO ()
display xs = do writeat (3, 2) (replicate 13 ' ')
                writeat (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons
                then process c xs
                else do beep
                        calc xs

beep :: IO ()
beep = putStr "\BEL"

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC" = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n" = eval xs
             | elem c "cC" = clear
             | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = return ()
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case P.parse expr xs of
            [(v, [])] -> calc (show v)
            _ -> do beep
                    calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

main :: IO ()
main = run
