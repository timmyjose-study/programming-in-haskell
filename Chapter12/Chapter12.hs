{-# LANGUAGE TemplateHaskell #-}

module Chapter12 where

import Control.Monad
import Data.Char
import Test.QuickCheck

{-
   Functors:

   Any type that can be mapped over.

   class Functor f where
    fmap :: (a -> b) -> f a -> f b

  -}

inc :: [Int] -> [Int]
inc [] = []
inc (n : ns) = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n : ns) = n ^ 2 : sqr ns

inc' :: [Int] -> [Int]
inc' = map (+ 1)

sqr' :: [Int] -> [Int]
sqr' = map (^ 2)

prop_inc' :: [Int] -> Bool
prop_inc' ns = inc ns == inc' ns

prop_sqr' :: [Int] -> Bool
prop_sqr' ns = sqr ns == sqr' ns

data List a = Nil | Cons a (List a)
  deriving (Show)

instance Functor List where
  -- fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

li :: List Int
li = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))

ls :: List String
ls = Cons "hello" (Cons "world" (Cons "we" (Cons "meet" (Cons "again" Nil))))

data Option a = None | Some a
  deriving (Show)

instance Functor Option where
  -- fmap :: (a -> b) -> Option a -> Option b
  fmap _ None = None
  fmap f (Some x) = Some (f x)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

instance Functor Tree where
  -- fmap  :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

ts :: Tree String
ts = Node (Node (Leaf "Hello") (Leaf "world")) (Node (Leaf "nice") (Node (Leaf "to") (Node (Leaf "meet") (Leaf "you"))))

{-
   instance Functor IO where
      fmap f mx = do
          x <- mx
          pure (f x)
   -}

inc'' :: Functor f => f Int -> f Int
inc'' = fmap (+ 1)

sqr'' :: Functor f => f Int -> f Int
sqr'' = fmap (^ 2)

{-
   Functor Laws:

   fmap id = id
   fmap (g . h) = fmap g . fmap h

   -}

{-
   Applicatives:

   Applicatives generalise the concept of function application.

   class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

    Typical usage:

    pure f <*> x1 <*> x2 ... <*> xn

    Now we can define an entire hierarchy of mapping functions:

    fmap0 :: a -> f a
    fmap0 = pure

    fmap1 :: (a -> b) -> f a -> f b
    fmap1 f x = pure f <*> x

    fmap2 :: (a -> b -> c) -> f a -> f b -> f c
    fmap2 f x y = pure f <*> x <*> y

    fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
    fmap3 f x y z = pure f <*> x <*> y <*> z

    and so on...

   -}

myAppend :: List a -> List a -> List a
myAppend Nil ys = ys
myAppend (Cons x xs) ys = Cons x (myAppend xs ys)

instance Applicative List where
  -- pure :: a -> List a
  pure = \x -> Cons x Nil

  -- (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  (Cons f fs) <*> xs = myAppend (fmap f xs) (fs <*> xs)

instance Applicative Option where
  -- pure :: a -> Option a
  pure = Some

  -- (<*>) :: Option (a -> b) -> Option a -> Option b
  (Some f) <*> (Some x) = Some (f x)
  _ <*> _ = None

instance Applicative Tree where
  -- pure :: a -> Tree a
  pure = \x -> Leaf x

  -- (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (Leaf f) <*> t = fmap f t
  (Node l r) <*> t = Node (l <*> t) (r <*> t)

prods :: [Int] -> [Int] -> [Int]
prods xs ys = pure (*) <*> xs <*> ys

{-
   instance Functor IO where
    pure = return

    mf <*> mx = do
      f <- mf
      x <- mx
      return (f x)
   -}

getChars :: Int -> IO String
getChars 0 = pure []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)

{-
   sequenceA :: Applicative f => [f a] -> f [a]
   sequenceA [] = pure []
   sequence (x : xs) = pure (:) <*> x <*> sequenceA xs
   -}

getChars' :: Int -> IO String
getChars' n = sequenceA (replicate n getChar)

{-
   Applicative Laws:

   pure id <*> x = x
   pure (g x) = pure g <*> pure x
   x <*> pure y = pure (\g -> g y) <*> x
   x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
   -}

{-
   Monads:

   Monads represent generalised effectful programming.

   class Applicative m => Monad m where
    (>>-) :: m a -> (a -> m b) -> m b
-}

data Expr = Val Int | Div Expr Expr
  deriving (Show)

--eval :: Expr -> Int
--eval (Val n) = n
--eval (Div e1 e2) = eval e1 `div` eval e2

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just $ m `div` n

--eval :: Expr -> Maybe Int
--eval (Val n) = Just n
--eval (Div e1 e2) =
--  case eval e1 of
--    Nothing -> Nothing
--    Just m -> case eval e2 of
--      Nothing -> Nothing
--      Just n -> safediv m n

--eval :: Expr -> Maybe Int
--eval (Val n) = Just n
--eval (Div e1 e2) =
--  eval e1 >>= \m ->
--    eval e2 >>= \n ->
--      safediv m n

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div e1 e2) = do
  m <- eval e1
  n <- eval e2
  safediv m n

instance Monad List where
  -- (>>=) :: List a -> (a -> List b) -> List b
  Nil >>= _ = Nil
  (Cons x xs) >>= f = myAppend (f x) (xs >>= f)

instance Monad Option where
  -- (>>=) :: Option a -> (a -> Option b) -> Option b
  None >>= _ = None
  (Some x) >>= f = f x

instance Monad Tree where
  -- (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  (Leaf x) >>= f = f x
  (Node l r) >>= f = Node (l >>= f) (r >>= f)

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
  x <- xs
  y <- ys
  pure (x, y)

{-
   Monad Laws:

   return x >>= f = f x
   mx >>= return = mx
   (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))
   -}

-- generic functions

{-
   mapM :: Monad m => (a -> m b) -> [a] -> m [b]
   mapM f [] = pure []
   mapM f (x : xs) = do
    y <- f x
    ys <- mapM f xs
    pure (y : ys)

   filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
   filterM p [] = pure []
   filterM p (x : xs) = do
    b <- p x
    ys <- filterM p xs
    pure (if b then x : ys else ys)
   -}

conv :: Char -> Maybe Int
conv c
  | isDigit c = Just (digitToInt c)
  | otherwise = Nothing

-- try running mapM conv "<input string>"

return []

main = $forAllProperties (quickCheckWithResult stdArgs {maxSuccess = 1000})