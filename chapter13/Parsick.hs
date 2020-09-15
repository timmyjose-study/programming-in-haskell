module Parsick where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) x = p x

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

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                         [] -> []
                         [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\_ -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         [(v, out)] -> [(v, out)])

-- core parsers

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (c : cs) -> [(c, cs)])

-- sequencing -> do 
-- choice -> <|>
-- repetition -> some, many
-- epsilon -> empty
-- success -> return / pure

-- primitive parsers

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

char :: Char -> Parser Char
char c = sat (== c)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

alpha :: Parser Char
alpha = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

-- derived parsers

space :: Parser ()
space = do many (sat (isSpace))
           return ()

ident :: Parser String
ident = do c <- lower
           cs <- many alpha
           return (c : cs)

nat :: Parser Int
nat = do ns <- some digit
         return (read ns)

int :: Parser Int
int = do char '-'
         ns <- nat
         return (-ns)
         <|> nat

string :: String -> Parser String
string [] = return []
string (c : cs) = do char c
                     string cs
                     return (c : cs)

-- higher-level parsers taking whitespace into consideration

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
symbol cs = token (string cs)