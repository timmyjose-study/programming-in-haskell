module IMonoids1

Semigroup Integer where
  -- (<+>) :: Integer -> Integer-> Integer
  (<+>) = (+)

Monoid Integer where
  -- neutral : Integer
  neutral = 0
