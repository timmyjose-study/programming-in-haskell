module Chapter10 where

readFirstAndThird :: IO (Char, Char)
readFirstAndThird = do
  f <- getChar
  getChar
  t <- getChar
  pure (f, t)

myGetLine :: IO String
myGetLine = do
  c <- getChar
  if c == '\n'
    then pure []
    else do
      cs <- myGetLine
      pure $ c : cs

myPutStr :: String -> IO ()
myPutStr [] = pure ()
myPutStr (c : cs) = do
  putChar c
  myPutStr cs

myPutStrLn :: String -> IO ()
myPutStrLn cs = do
  myPutStr cs
  putChar '\n'

strlen :: IO ()
strlen = do
  putStr "Enter a line: "
  line <- myGetLine
  myPutStrLn $ "The length of \"" ++ line ++ "\" is " ++ show (length line)