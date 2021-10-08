module MonadicParsing where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item =
  P
    ( \s -> case s of
        [] -> []
        v : out -> [(v, out)]
    )

three :: Parser (Char, Char)
three =
  P
    ( \s -> case parse item s of
        [] -> []
        [(x, out)] -> case parse item out of
          [] -> []
          [(_, out)] -> case parse item out of
            [] -> []
            [(z, out)] -> [((x, z), out)]
    )

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p =
    P
      ( \s -> case parse p s of
          [] -> []
          [(v, out)] -> [(f v, out)]
      )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure = \x -> P (\s -> [(x, s)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px =
    P
      ( \s -> case parse pf s of
          [] -> []
          [(f, out)] -> parse (fmap f px) out
      )

threeA :: Parser (Char, Char)
threeA = pure f <*> item <*> item <*> item
  where
    f x y z = (x, z)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  px >>= f =
    P
      ( \s -> case parse px s of
          [] -> []
          [(x, out)] -> parse (f x) out
      )

threeM :: Parser (Char, Char)
threeM = do
  x <- item
  item
  z <- item
  return (x, z)

{-
  classs Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    many :: f a -> f [a]
    some :: f a -> f [a]

    many x = some x <|> pure []
    some x = pure (:) <*> x <*> many x

  Laws:

  empty <|> x = x
  x <|> empty = x
  x <|> (y <|> z) = (x <|> y) <|> z
-}

data Option a = None | Some a
  deriving (Show)

instance Functor Option where
  -- fmap :: (a -> b) -> Option a -> Option b
  fmap _ None = None
  fmap f (Some x) = Some (f x)

instance Applicative Option where
  -- pure :: Option a
  pure = Some

  -- (<*>) :: Option (a -> b) -> Option a -> Option b
  None <*> _ = None
  (Some f) <*> mx = fmap f mx

instance Alternative Option where
  -- empty :: Option a
  empty = None

  -- (<|>) :: Option a -> Option a -> Option a
  None <|> my = my
  mx <|> _ = mx

instance Monad Option where
  -- (>>=) :: Option a -> (a -> Option b) -> Option b
  None >>= _ = None
  (Some x) >>= f = f x

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\_ -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    P
      ( \s -> case parse p s of
          [] -> parse q s
          [(v, out)] -> [(v, out)]
      )

-- deriving more parsers from the basic parsers (item for a single item, pure for always success, and empty for always fail) and the
-- sequencing and choice operators

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then pure x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = pure []
string (c : cs) = do
  char c
  string cs
  pure (c : cs)

ident :: Parser String
ident = do
  c <- lower
  cs <- many alphanum
  pure (c : cs)

nat :: Parser Int
nat = do
  ns <- some digit
  pure (read ns)

space :: Parser ()
space = do
  many (sat isSpace)
  pure ()

int :: Parser Int
int =
  do
    char '-'
    n <- nat
    pure (n)
    <|> nat

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  pure v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol cs = token (string cs)

nats :: Parser [Int]
nats = do
  symbol "["
  n <- natural
  ns <- many (do symbol ","; natural)
  symbol "]"
  pure (n : ns)