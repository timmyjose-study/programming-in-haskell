module StateMonad where

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f st = S (\s -> let (x, s') = app st s in (f x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- <*> :: ST (a -> b) -> ST a -> ST b
  stf <*> stx =
    S
      ( \s ->
          let (f, s') = app stf s
              (x, s'') = app stx s'
           in (f x, s'')
      )

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  stx >>= f =
    S
      ( \s ->
          let (x, s') = app stx s
              (out, s'') = app (f x) s'
           in (out, s'')
      )

-- relabelling trees using the ST monad

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = pure Leaf <*> fresh
alabel (Node l r) = pure Node <*> alabel l <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do
  n <- fresh
  pure (Leaf n)
mlabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  pure (Node l' r')
