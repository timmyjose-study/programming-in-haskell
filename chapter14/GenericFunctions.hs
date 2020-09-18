{-# LANGUAGE GADTs #-}

module Genericfunctions where

import Data.Foldable
import Data.Monoid

average :: [Int] -> Int
average ns = sum ns `div` length ns

average' :: Foldable t => t Int -> Int
average' ns = sum ns `div` length ns

data Tree a where
  Leaf :: a -> Tree a
  Node :: Tree a -> Tree a -> Tree a
  deriving (Eq, Ord, Read, Show)

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold (Leaf x) = x
  fold (Node l r) = fold l <> fold r

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l <> foldMap f r

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f v (Leaf x) = f x v
  foldr f v (Node l r) = foldr f (foldr f v r) l

  -- foldl :: (b -> a -> b) -> b -> Tree a -> b
  foldl f v (Leaf x) = f v x
  foldl f v (Node l r) = foldl f (foldl f v l) r

  {-
     In Data.Foldable:

     and :: Foldable t => t Bool -> Bool
     and = getAll . foldMap All

     or :: Foldable t => t Bool -> Bool
     or = getAny . foldMap Any

     all :: Foldable t => (a -> Bool) -> t a -> Bool
     all p = getAll . foldMap (All . p)

     any :: Foldable t => (a -> Bool) -> t a -> Bool
     any p = getAny . foldMap (Any . p)
  -}