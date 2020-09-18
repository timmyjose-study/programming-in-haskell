{-# LANGUAGE GADTs #-}

module Chapter14 where

import Data.Foldable
import Data.Monoid

  {-

     Monoids:

     A Semigroup is a set with an associative operator. A monoid is an extension of a Semigroup.  
     A monoid is a set with an associative operator and an identity element for that set. Eg; { Z, +, 0 } and { Z, *, 1 } are monoids.

     class Semigroup a where
        (<>) :: a -> a -> a

     class Semigroup a => Monoid a where
        mempty :: a -- identity element
        mappend :: a -> a -> a -- associative operator

        mconcat :: [a] -> a
        mconcat = foldr mappend mempty


      Laws:

        mempty `mappend` x = x
        x `mappend` mempty = x
        x `mappend` (y `mappaned` z) = (x `mappend` y) `mappend` z


      instance Monoid [a] where
        -- mempty :: [a]
        mempty = []

        -- mappend :: [a] -> [a] -> [a]
        mappend = (++)


      instance Monoid a => Monoid (Maybe a) where
        -- mempty :: Maybe a
        mempty = Nothing

        -- mappend :: Maybe a -> Maybe a -> Maybe a
        Nothing `mappend` my = my
        mx `mappend` Nothing = mx
        (Just x) `mappend` (Just y) = Just (x `mappend` y)

  -}

--instance Monoid Int where
--  -- mempty :: Int
--  mempty = 0
--
--  -- mappend :: Int -> Int -> Int
--  mappend = (+)
--
--instance Monoid Int where
--  -- mempty :: Int
--  mempty = 1
--
--  -- mappend :: Int -> Int -> Int
--  mappend = (*)

-- Haskell only allows a single typeclass-concrete type combination, so the above cannot be declared. As a workaround, we have to make do with
-- wrapper classes - Sum a and Product a in Data.Monoid

newtype MySum a = MySum a deriving (Eq, Ord, Read, Show)

myGetSum :: MySum a -> a
myGetSum (MySum x) = x

-- now we can make this wrapper type a monoid

instance Num a => Semigroup (MySum a) where
  -- (<>) :: MySum a -> MySum a -> MySum a
  MySum x <> MySum y = MySum (x + y)

instance Num a => Monoid (MySum a) where
  -- mempty :: MySum a
  mempty = MySum 0

  -- mappend :: MySum a -> MySum a -> MySum a
  MySum x `mappend` MySum y = MySum (x + y)

-- likewise to make { Z, *, 1 } a monoid

newtype MyProduct a = MyProduct a deriving (Eq, Ord, Read, Show)

myGetProduct :: MyProduct a -> a
myGetProduct (MyProduct x) = x

instance Num a => Semigroup (MyProduct a) where
  -- (<>) :: MyProduct a -> MyProduct a -> MyProduct a
  MyProduct x <> MyProduct y = MyProduct (x * y)

instance Num a => Monoid (MyProduct a) where
  -- mempty :: MyProduct a
  mempty = MyProduct 1

  -- mappend :: MyProduct a -> MyProduct a -> MyProduct a
  MyProduct x `mappend` MyProduct y = MyProduct (x * y)

{-
  
    Foldables:

    class Foldable t where
      fold :: Monoid a => t a -> a
      foldMap : Monoid b => (a -> b) - t a -> b
      foldr :: (a -> b -> b) -> b -> t a -> b
      foldl :: (b -> a -> b) -> b -> t a -> b
  
     Minimal definition must include `foldMap` or `foldr`
-}

foldList :: Monoid a => [a] -> a
foldList [] = mempty
foldList (x : xs) = x <> foldList xs

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

foldTree :: Monoid a => Tree a -> a
foldTree (Leaf x) = x
foldTree (Node l r) = foldTree l <> foldTree r

data List a where
  Nil :: List a
  Cons :: a -> List a -> List a
  deriving (Eq, Ord, Read, Show)

li1 :: List (Sum Int)
li1 = Cons (Sum 1) (Cons (Sum 2) (Cons (Sum 3) (Cons (Sum 4) (Cons (Sum 5) Nil))))

li2 :: List (Product Int)
li2 = Cons (Product 1) (Cons (Product 2) (Cons (Product 3) (Cons (Product 4) (Cons (Product 5) Nil))))

instance Foldable List where
  -- fold :: Monoid a -> List a -> a
  fold Nil = mempty
  fold (Cons x xs) = x <> fold xs

  -- foldMap :: Monoid b => (a -> b) -> List a -> b
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

  -- foldr :: (a -> r -> b) -> b -> List a -> b
  foldr _ v Nil = v
  foldr f v (Cons x xs) = f x (foldr f v xs)

  -- foldl :: (b -> a -> b) -> b -> List a -> b
  foldl _ v Nil = v
  foldl f v (Cons x xs) = foldl f (f v x) xs

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold (Leaf x) = x
  fold (Node l r) = fold l <> fold r

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l <> foldMap f r

  -- foldr :: (a -> b -> b) -> b -> List a -> b
  foldr f v (Leaf x) = f x v
  foldr f v (Node l r) = foldr f (foldr f v r) l -- right to left

  -- foldl :: (b -> a -> b) -> b -> List a -> b
  foldl f v (Leaf x) = f v x
  foldl f v (Node l r) = foldl f (foldl f v l) r -- left to right

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

  {-
    Other primitives and defaults in Foldable:

    null :: t a -> Bool
    length :: t a -> Int
    elem :: Eq a => a -> t a -> Bool
    maximum :: Ord a => t a -> a
    minimum :: Ord a => t a -> a
    sum :: Num a => t a -> a
    product :: Num a => t a -> a

    foldr1 :: (a -> a -> a) -> t a -> a
    foldl1 :: (a -> a -> a) -> t a -> a

    toList :: t a -> [a]
  -}
