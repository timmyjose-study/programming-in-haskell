module LazyEvaluation where

{-
  The evaluation model of Haskell is Call by Name with structural sharing, and this is known as Lazy Evaluation.
-}

primes :: [Int]
primes = sieve [2 ..]

sieve :: [Int] -> [Int]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- strictness under lazy evaluation

sumwith :: Int -> [Int] -> Int
sumwith acc [] = acc
sumwith acc (n : ns) = sumwith (acc + n) ns

sumwithStrict :: Int -> [Int] -> Int
sumwithStrict acc [] = acc
sumwithStrict acc (n : ns) = (sumwith $! (acc + n)) ns