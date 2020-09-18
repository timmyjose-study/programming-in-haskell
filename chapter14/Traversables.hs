{-# LANGUAGE GADTs #-}

module Traversable where

myTraverse :: (a -> Maybe b) -> [a] -> Maybe [b]
myTraverse f [] = pure []
myTraverse f (x : xs) = pure (:) <*> f x <*> myTraverse f xs

dec :: Int -> Maybe Int
dec 0 = Nothing
dec n = Just (n - 1)

  {-
    class (Functor t, Foldable t) => Traversable t where
      traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
      sequenceA :: Applicative f => t (f a) -> f (t a)
      mapM :: Monad m => (a -> m b) -> t a -> m (t b)
      sequence :: Monad m => t (m a ) -> m (t a)

      sequenceA = traverse id
      mapM = traverse
      sequence = sequenceA

  -}

data List a where
  Nil :: List a 
  Cons :: a -> List a -> List a 
  deriving (Eq, Ord, Read, Show)

instance Functor List where
  -- fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  -- foldMap :: Monoid b => (a -> b) -> List a -> b
  foldMap f Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  -- traverse :: Applicative f => (a -> f b) -> List a -> List (f b)
  traverse f Nil = pure Nil
  traverse f (Cons x xs) = pure Cons <*> f x <*> traverse f xs

data Tree a where
  Leaf :: a -> Tree a
  Node :: Tree a -> Tree a -> Tree a
  deriving (Eq, Ord, Read, Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Foldable Tree where
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l <> foldMap f r

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> Tree (f b)
  traverse f (Leaf x) = pure Leaf <*> f x
  traverse f (Node l r) = pure Node <*> traverse f l <*> traverse f r