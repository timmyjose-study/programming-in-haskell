module Chapter1 where

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (n : ns) = n + mySum ns

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x : xs) = qsort smaller ++ [x] ++ qsort bigger
  where
    smaller = [a | a <- xs, a <= x]
    bigger = [b | b <- xs, b > x]

seqn :: Monad m => [m a] -> m [a]
seqn [] = pure []
seqn (act : acts) = do
  x <- act
  xs <- seqn acts
  pure $ x : xs