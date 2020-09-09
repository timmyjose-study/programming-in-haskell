module Ex10 where

import Data.Char (isDigit)

-- 1.

myPutStr :: String -> IO ()
myPutStr cs = sequence_ [putChar c | c <- cs]

-- 4.

isNumber :: String -> Bool
isNumber = all isDigit

readNums :: Int -> IO [Int]
readNums n = aux n []
  where
    aux 0 acc = return acc
    aux n acc = do ns <- getLine
                   if isNumber ns
                      then aux (n - 1) (read ns : acc)
                      else do putStrLn "error: please enter a number: "
                              aux n acc

adder :: IO ()
adder = do putStr "How many numbers? "
           ns <- getLine
           if isNumber ns 
              then let n = read ns in
                       do nums <- readNums n
                          putStrLn $ "The total is " ++ show (sum nums)
           else
            do putStrLn "error: please enter a number"
               adder

-- 5.

readNumber :: IO Int
readNumber = do ns <- getLine
                if isNumber ns
                   then return (read ns)
                   else do putStrLn "error: please enter a number:"
                           readNumber

adder' :: IO ()
adder' = do putStr "How many numbers? "
            ns <- getLine
            if isNumber ns 
               then let n = read ns in
                        do nums <- sequence (replicate n readNumber)
                           putStrLn $ "The total is " ++ show (sum nums)
               else do putStrLn "error: please enter a number"
                       adder'