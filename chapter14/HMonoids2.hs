module HMonoids2 where

instance Semigroup Int where
  (<>) = (*)

instance Monoid Int where
  mempty = 1
  mappend = (*)
