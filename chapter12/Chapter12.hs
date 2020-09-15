{- Monads, Applicatives, and Functors -}

module Chapter12 where

inc :: [Int] -> [Int]
inc [] = []
inc (n : ns ) = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n : ns) = n^2 : sqr ns

inc1 :: [Int] -> [Int]
inc1 = map (+1) 

sqr1 :: [Int] -> [Int]
sqr1 = map (^2)

  {-
     class Functor f where
        fmap :: (a -> b) -> f a -> f b

    
     instance Functor [] where
        -- fmap :: (a -> b) -> [a] -> [b]
        fmap = map

     instance Functor Maybe where
      -- fmap :: (a -> b) -> Maybe a -> Maybe b
      fmap f Nothing = Nothing
      fmap f (Just x) = Just (f x)

     instance Functor IO where
        -- fmap :: (a -> b) -> IO a -> IO b
        fmap f mx = do { x <- mx; return (f x) }
  -}

data Option a = None | Some a deriving Show

instance Functor Option where
  -- fmap :: (a -> b) -> Option a -> Option b
  fmap f None = None
  fmap f (Some x) = Some (f x)


data List a = Nil | Cons a (List a) deriving Show

instance Functor List where
  -- fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

ls :: List String
ls = Cons "Hello" (Cons "World" (Cons "Nice" (Cons "To" (Cons "Meet" (Cons "You" (Cons "Again" Nil))))))


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

ti :: Tree Int
ti = Node (Leaf 10) (Node (Leaf 12) (Node (Leaf 13) (Leaf 14)))

inc2 :: Functor f => f Int -> f Int
inc2 = fmap (+1)

  {-
     Functor Laws:

     fmap id = id

     fmap (g . h) = fmap g . fmap h

     -}

  {-

     fmap0 :: a -> f a
     fmap1 :: (a -> b) -> f a -> f b
     fmap2 :: (a -> b -> c) -> f a -> f b -> f c
     
     class Functor f => Applicative f where
        pure :: a -> f a

        (<*>) :: f (a -> b) -> f a -> f b

    fmap0 :: a -> f a
    fmap0 = pure

    fmap1 :: (a -> b) -> f a -> f b
    fmap1 f x = pure f <*> x

    fmap2 :: (a -> b -> c) -> f a -> f b -> f c
    fmap2 f x y = pure f <*> x <*> y

    instance Applicative IO where
      -- pure :: a -> f a
      pure = return

      -- (<*>) :: IO (a -> b) -> IO a -> IO b
      mf <*> mx = do { f <- mf; x <- mx; return (f x) }

  -}

instance Applicative Option where
  -- pure :: a -> Maybe a
  pure = Some

  -- (<*>) :: Option (a -> b) -> Option a -> Option b
  None <*> _ = None
  (Some f) <*> mx = fmap f mx

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

li :: List Int
li = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

instance Applicative List where
  -- pure :: a -> List a
  pure = \x -> Cons x Nil

  -- (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x * y | x <- xs, y <- ys]

prods1 :: Applicative f => f Int -> f Int -> f Int
prods1 xs ys = pure (*) <*> xs <*> ys

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)

mySequencea :: Applicative f => [f a] -> f [a]
mySequencea [] = pure []
mySequencea (f : fs) = pure (:) <*> f <*> mySequencea fs

getChars1 :: Int -> IO String
getChars1 n = sequenceA (replicate n getChar)

  {-

     pure id <*> x = x
     pure (g x) = pure g <*> pure x
     x <*> pure y = pure (\g -> g y) <*> x
     x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

     g <$> x = pure f <*> x = fmap f x
  -}

  {-
     class Applicative m => Monad m where
       return :: a -> m a

       (>>=) :: m a -> (a -> m b) -> m b

       return = pure
  -}

data Expr = Val Int | Div Expr Expr deriving Show

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _  0 = Nothing
safediv x y = Just (x `div` y)

eval1 :: Expr -> Maybe Int
eval1 (Val n) = Just n
eval1 (Div x y) = case eval1 x of
                    Nothing -> Nothing
                    Just a -> case eval1 y of
                                Nothing -> Nothing
                                Just b -> safediv a b

eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Div x y) = eval2 x >>= \a -> 
                    eval2 y >>= \b ->
                      safediv a b

eval3 :: Expr -> Maybe Int
eval3 (Val n) = Just n
eval3 (Div x y) = do a <- eval3 x
                     b <- eval3 y
                     safediv a b

instance Monad Option where
  -- (>>=) :: Option a -> (a -> Option b) -> Option b
  None >>= _ = None
  Some x >>= f = f x

instance Monad List where
  -- (>>=) :: List a -> (a -> List b) -> List b
  Nil >>= _ = Nil
  (Cons x xs) >>= f = append (f x) (xs >>= f)

pairs :: List a -> List b -> List (a, b)
pairs xs ys = do x <- xs
                 y <- ys
                 return (x, y)