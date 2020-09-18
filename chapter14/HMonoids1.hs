module HMonoids1 where

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0
  mappend = (+)