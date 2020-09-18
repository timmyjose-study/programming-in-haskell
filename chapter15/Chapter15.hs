{- Lazy Evaluation -}

module Chapter15 where

  {-
    Innermost evaluation (not withing lambda expressions)-> call by value 
    Outermost evaluation (not within lambda expressions) -> call by name

    Some functions require that their arguments be fully evaluated even when using outermost evaluation. Such functions are called "strict" functions.

    


  -}

inf :: Int
inf = 1 + inf

  {-
     Calling fst on (0, inf) gives 0 in the case of a call-by-name language and leads to non-termination in the case of a call-by-value language.
     Call by name has the property that for any expression that does terminate, call-by-name is guaranteed to find it.
  -}

square :: Int -> Int
square n = n * n

  {-
    Call by Value:

    square (1 + 2) -> square 3 -> 3 * 3 -> 9. Number of steps = 3.

    Call by Name:

    square (1 + 2) -> (1 + 2) * (1 + 2) -> 3  * (1 + 2) -> 3 * 3 -> 9. Number of steps = 4.

    Call by value ensures that arguments are evaluated exactly once. Call by name may evaluate arguments more than once.

    However, we can use sharing to mitigate the problem of multiple evaluation in the case of call by name.

    The use of call by name along with sharing is called "lazy evaluation".

  -}

primes :: [Int]
primes = sieve [2 ..]

sieve :: [Int] -> [Int]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

  {- Strict application 

     f $! x implies strict application of the function f to the argument x. Note that the evaluation of the argument proceeds in a call by value manner
     only so long as the argument is sufficiently evaluated for normal lazy evaluation to proceed.

     Given a function application with two arguments, f x y, we can have three different behaviours:

     (f $! x) y -> strict evaluation of x
     f x ($! y) -> strict evaluation of y
     (f $! x) $! y -> strict evaluation of both x and y

     In Haskell, strict evaluation is mostly used to improve the space performance of programs, as shown in the
     example below.

  -}

sumwith :: Int -> [Int] -> Int
sumwith acc [] = acc
sumwith acc (n : ns) = sumwith (acc + n) ns

-- the strict equivalent is

sumwith' :: Int -> [Int] -> Int
sumwith' acc [] = acc
sumwith' acc (n : ns) = (sumwith' $! (acc + n)) ns