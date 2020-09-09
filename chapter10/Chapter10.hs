{- Interactive Programming -}

  {-

     type IO = World -> World

     type IO a = World -> (a, World)

     data IO a = ...

     getChar :: IO Char
     putChar :: Char -> IO ()
     return :: a -> IO a

     -}

module Chapter10 where

act :: IO (Char, Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x, y)

myGetLine :: IO String
myGetLine = do c <- getChar
               if c == '\n' 
                  then return []
                  else do cs <- myGetLine
                          return (c : cs)

myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (c : cs) = do putChar c
                       myPutStr cs

myPutStrLn :: String -> IO ()
myPutStrLn cs = do myPutStr cs
                   putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string: "
            string <- getLine
            let len = length string in
                do putStrLn $ "The length of \"" ++ string ++ "\" is " ++ show len
