{-# LANGUAGE GADTs #-}

module ArithExpr where

import Control.Applicative
import Data.Char

-- the monadic parser

data Parser a where
  MkParser :: (String -> [(a, String)]) -> Parser a

parse :: Parser a -> String -> [(a, String)]
parse (MkParser p) inp = p inp

item :: Parser Char
item =
  MkParser
    ( \inp -> case inp of
        [] -> []
        v : out -> [(v, out)]
    )

three :: Parser (Char, Char)
three =
  MkParser
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
    MkParser
      ( \s -> case parse p s of
          [] -> []
          [(v, out)] -> [(f v, out)]
      )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = MkParser (\s -> [(x, s)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px =
    MkParser
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
  p >>= f =
    MkParser
      ( \s -> case parse p s of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

threeM :: Parser (Char, Char)
threeM = do
  x <- item
  item
  z <- item
  pure (x, z)

instance Alternative Parser where
  -- empty :: Parser a
  empty = MkParser (\_ -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    MkParser
      ( \s -> case parse p s of
          [] -> parse q s
          [(v, out)] -> [(v, out)]
      )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then pure x else empty

digit :: Parser Char
digit = sat isDigit

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

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
    pure (-n)
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

-- Arithmetic expressions

{-
  Grammar:

    expr ::= term ((+ | -) expr | epsilon)
    term ::= factor ((* | /) expr | epsilon)
    factor ::= ( expr ) | integer
    integer ::= ... | -2 | -1 | 0 | 1 | 2 | ...
-}

expr :: Parser Int
expr =
  do
    t <- term
    do
      symbol "+"
      e <- expr
      pure (t + e)
      <|> do
        symbol "-"
        e <- expr
        pure (t - e)
      <|> pure t

term :: Parser Int
term = do
  f <- factor
  do
    symbol "*"
    t <- term
    pure (f * t)
    <|> do
      symbol "/"
      t <- term
      pure (f `div` t)
    <|> pure f

factor :: Parser Int
factor =
  do
    symbol "("
    e <- expr
    symbol ")"
    pure e
    <|> integer

eval :: String -> Int
eval inp = case parse expr inp of
  [] -> error "Invalid input"
  [(res, [])] -> res
  [(_, out)] -> error $ "Unused input" ++ out