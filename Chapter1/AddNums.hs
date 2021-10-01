module Main where

import Data.Char (isDigit)

readNum :: IO (Maybe Int)
readNum = do
  inp <- getLine
  case all isDigit inp of
    False -> pure Nothing
    True -> pure $ Just (read inp)

main :: IO ()
main = do
  m <- readNum
  case m of
    Nothing -> putStrLn "Invalid first number"
    Just mm -> do
      n <- readNum
      case n of
        Nothing -> putStrLn "Invalid second number"
        Just nn -> putStrLn $ "The sum of " ++ show mm ++ " and " ++ show nn ++ " is " ++ show (mm + nn)