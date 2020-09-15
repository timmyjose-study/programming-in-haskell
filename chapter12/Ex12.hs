module Ex12 where

-- 1.

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

-- 7.

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var a) = Var (f a)
  fmap _ (Val n) = Val n
  fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure = Var

  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  Var f <*> x = fmap f x
  _ <*> Val n = Val n

instance Monad Expr where
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Var x >>= f = f x
  Val n >>= _ = Val n