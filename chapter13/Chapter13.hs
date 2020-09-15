{- Monadic Parsing -}

module Chapter13 where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

-- the most basic parser

item :: Parser Char
item = P (\inp -> case inp of
                        [] -> []
                        (x : xs) -> [(x, xs)])

-- sequencing parsers

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\inp -> case parse p inp of
                          [] -> []
                          [(v, out)] -> [(f v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P (\inp -> case parse pf inp of
                          [] -> []
                          [(f, out)] -> parse (fmap f px) out)

three :: Parser (Char, Char)
three = pure f <*> item <*> item <*> item
    where
      f x y z = (x, z)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                         [] -> []
                         [(v, out)] -> parse (f v) out)

three' :: Parser (Char, Char)
three' = do x <- item
            item
            z <- item
            return (x, z)

-- making choices

  {-
     class Applicative f => Alternative f where
        empty :: f a
        (<|>) :: f a -> f a -> f a
        many :: f a -> f [a]
        some :: f a -> f [a]

        many x = some x <|> pure []
        some x = pure (:) <*> x <*> many x


     empty <|> x = x
     x <|> empty = x
     x <|> (y <|> z) = (x <|> y) <|> z

  -}

data Option a = None | Some a deriving (Eq, Ord, Read, Show)

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

instance Monad Option where
  -- (>>=) :: Option a -> (a -> Option b) -> Option b
  None >>= _ = None
  (Some x) >>= f = f x

instance Alternative Option where
  -- empty :: Option a
  empty = None

  -- (<|>) :: Option a -> Option a -> Option a
  None <|> my = my
  mx <|> _ = mx

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\_ -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         [(v, out)] -> [(v, out)])

-- derived primnitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

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
string [] = return []
string (c : cs) = do char c
                     string cs
                     return (c : cs)

-- repetitions for matches

ident :: Parser String
ident = do c <- lower
           cs <- many alphanum
           return (c : cs)

nat :: Parser Int
nat = do ns <- some digit
         return (read ns)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         ns <- nat
         return (-ns)
      <|> nat

-- handling spacing

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ", "; natural)
          symbol "]"
          return (n : ns)
