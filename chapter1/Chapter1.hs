{- Introduction -}

module Chapter1 where

double :: Num a => a -> a
double x = x + x

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (n : ns) = n + mySum ns

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort bigger
  where
    smaller = [a | a <- xs, a <= x]
    bigger = [b | b <- xs, b > x]

seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act : acts) = do x <- act
                       xs <- seqn acts
                       return (x : xs)
