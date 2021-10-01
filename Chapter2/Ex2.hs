module Ex2 where

myLast :: [a] -> a
myLast = head . reverse

myLast' :: [a] -> a
myLast' = head . take 1 . reverse

myInit :: [a] -> [a]
myInit = reverse . tail . reverse

myInit' :: [a] -> [a]
myInit' xs = take (length xs - 1) xs
