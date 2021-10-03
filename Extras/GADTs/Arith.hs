{-# LANGUAGE GADTs #-}

module Arith where

--data Expr = I Int | B Bool | Add Expr Expr | Mul Expr Expr | Eq Expr Expr
--  deriving (Show)
--
---- (5 + 1) * 7
--e1 :: Expr
--e1 = Mul (Add (I 5) (I 1)) (I 7)
--
--eval :: Expr -> Maybe (Either Int Bool)
--eval (I n) = Just $ Left n
--eval (B b) = Just $ Right b
--eval (Add e1 e2) = do
--  l <- eval e1
--  r <- eval e2
--  case l of
--    Right _ -> Nothing
--    Left m -> case r of
--      Right _ -> Nothing
--      Left n -> Just $ Left (m + n)
--eval (Mul e1 e2) = do
--  l <- eval e1
--  r <- eval e2
--  case l of
--    Right _ -> Nothing
--    Left m -> case r of
--      Right _ -> Nothing
--      Left n -> Just $ Left (m * n)
--eval (Eq e1 e2) = do
--  l <- eval e1
--  r <- eval e2
--  case l of
--    Left x -> case r of
--      Left y -> Just $ Right (x == y)
--      Right _ -> Nothing
--    Right x -> case r of
--      Left _ -> Nothing
--      Right y -> Just $ Right (x == y)

-- phantom types

--data Expr a
--  = I Int
--  | B Bool
--  | Add (Expr a) (Expr a)
--  | Mul (Expr a) (Expr a)
--  | Eq (Expr a) (Expr a)
--  deriving (Show)

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq :: Eq a => Expr a -> Expr a -> Expr Bool

e1 :: Expr Int
e1 = Add (Mul (I 2) (I 3)) (Add (I 10) (I 2))

b1 :: Expr Bool
b1 = Eq (I 2) (Add (I 1) (I 1))

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Eq x y) = eval x == eval y

-- data List a = Nil | Cons a (List a) deriving (Show)

data List a where
  Nil :: List a
  Cons :: a -> List a -> List a

--data Option a = None | Some a deriving (Show)

data Option a where
  None :: Option a
  Some :: a -> Option a

--data RoseTree a = RoseTree a [RoseTree a] deriving (Show)

data RoseTree a where
  RoseTree :: a -> [RoseTree a] -> RoseTree a

data Empty

data NonEmpty

data SafeList a b where
  Null :: SafeList a Empty
  Kons :: a -> SafeList a b -> SafeList a NonEmpty

safeHead :: SafeList a NonEmpty -> a
safeHead (Kons x _) = x

-- the problem with GADTs - too much specialisation
--silly :: Bool -> SafeList a b
silly False = Null
silly True = Kons () Null