{-# LANGUAGE GADTs #-}

module FoldablesAndFriends where

import Data.Foldable
import Data.Monoid

{-
  class Semigroup a where
    (<>) :: a -> a -> a

  class Semigroup a => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mappend = (<>)

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

  Laws:

  mempty `mappend` x = x
  x `mappend` mempty = x
  x `mappend (y `mappend` z) = (x `mappend` y) `mappend` z
-}

data List a where
  Nil :: List a
  Cons :: a -> List a -> List a

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

instance Semigroup (List a) where
  -- (<>) :: a -> a -> a
  (<>) = append

instance Monoid (List a) where
  -- mempty :: a
  mempty = Nil

data Option a where
  None :: Option a
  Some :: a -> Option a

instance Semigroup a => Semigroup (Option a) where
  -- (<>) :: a -> a -> a
  None <> my = my
  mx <> None = mx
  (Some x) <> (Some y) = Some (x <> y)

{-

  Haskell does not allow more than one instance for any given class + type combination, so these do not work,
  even if they are logically correct. The solution is to introduce wrapper classes.

instance Monoid a => Monoid (Option a) where
  -- mempty :: Optio a
  mempty = None

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0

instance Semigroup Int where
  (<>) = (*)

instance Monoid Int where
  mempty = 1
  -}

data MySum a where
  MySum :: a -> MySum a
  deriving (Eq, Show, Read, Ord)

getMySum :: MySum a -> a
getMySum (MySum x) = x

instance Num a => Semigroup (MySum a) where
  -- (<>) :: MySum a -> MySum a -> MySum a
  MySum x <> MySum y = MySum (x + y)

instance Num a => Monoid (MySum a) where
  -- mempty :: MySum a
  mempty = MySum 0

data MyProduct a where
  MyProduct :: a -> MyProduct a
  deriving (Eq, Show, Read, Ord)

instance Num a => Semigroup (MyProduct a) where
  -- (<>) :: MyProduct a -> MyProduct a -> MyProduct a
  MyProduct x <> MyProduct y = MyProduct (x * y)

instance Num a => Monoid (MyProduct a) where
  -- mempty :: MyProduct a -> MyProduct a -> MyProduct a
  mempty = MyProduct 1

data MyAll where
  MyAll :: Bool -> MyAll
  deriving (Eq, Ord, Read, Show)

instance Semigroup MyAll where
  -- (<>) :: MyAll -> MyAll -> MyAll
  MyAll x <> MyAll y = MyAll (x && y)

instance Monoid MyAll where
  -- mempty :: MyAny
  mempty = MyAll False

data MyAny where
  MyAny :: Bool -> MyAny
  deriving (Eq, Ord, Read, Show)

instance Semigroup MyAny where
  -- (<>) :: MyAny -> MyAny -> MyAny
  MyAny x <> MyAny y = MyAny (x || y)

instance Monoid MyAny where
  -- mempty :: MyAny
  mempty = MyAny True

-- foldables

myFold :: Monoid a => [a] -> a
myFold [] = mempty
myFold (x : xs) = x <> myFold xs

data Tree a where
  Leaf :: a -> Tree a
  Node :: Tree a -> Tree a -> Tree a
  deriving (Eq, Ord, Read, Show)

foldTree :: Monoid a => Tree a -> a
foldTree (Leaf x) = x
foldTree (Node l r) = foldTree l <> foldTree r

tree :: Tree (Sum Int)
tree = Node (Node (Leaf (Sum 10)) (Leaf (Sum 20))) (Node (Leaf 30) (Node (Leaf 40) (Leaf 50)))

{-
  Data.Foldable:

  class Foldable t where
    fold :: Monoid a => t a -> a
    foldMap :: Monoid b => (a -> b) -> t a -> b
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b

    null :: t a -> Bool
    length :: t a -> Int
    elem :: Eq a => a -> t a -> Bool
    maximum :: Ord a => t a -> a
    minimum :: Ord a => t a -> a
    sum :: Num a => t a -> a
    product :: Num a => t a -> a

    foldr1 :: (a -> a -> a) -> t a -> a
    foldl1 :: (a -> a -> a) -> t a -> a

    toList :; t a -> [a]
-}

instance Foldable List where
  -- fold :: Monoid a => List a -> a
  fold Nil = mempty
  fold (Cons x xs) = x <> fold xs

  -- foldMap :: Monoid b => (a -> b) -> List a -> b
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

  -- foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ acc Nil = acc
  foldr f acc (Cons x xs) = f x (foldr f acc xs)

  -- foldl :: (b -> a -> b) -> b -> List a -> b
  foldl _ acc Nil = acc
  foldl f acc (Cons x xs) = foldl f (f acc x) xs

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold (Leaf x) = x
  fold (Node l r) = fold l <> fold r

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l <> foldMap f r

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f acc (Leaf x) = f x acc
  foldr f acc (Node l r) = foldr f (foldr f acc r) l

  -- foldl :: (b -> a -> b) -> b -> Tree a -> b
  foldl f acc (Leaf x) = f acc x
  foldl f acc (Node l r) = foldl f (foldl f acc l) r

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

{-
  Traversables:

  class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id

    mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    mapM = traverse

    sequence :: Monad m => t (m a) -> m (t a)
    sequence = sequenceA
-}

myTraverse :: (a -> Maybe b) -> [a] -> Maybe [b]
myTraverse f [] = pure []
myTraverse f (x : xs) = pure (:) <*> f x <*> myTraverse f xs

dec :: Int -> Maybe Int
dec n
  | n > 0 = Just (n - 1)
  | otherwise = Nothing

instance Functor List where
  -- fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Traversable List where
  -- traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = pure Cons <*> f x <*> traverse f xs

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf x) = pure Leaf <*> f x
  traverse f (Node l r) = pure Node <*> traverse f l <*> traverse f r