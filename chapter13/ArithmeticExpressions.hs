module ArithmeticExpressions where

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

-- sequence -> do from Monad
-- choice -> <|> from Alternative
-- epsilon -> empty from Alternative
-- alway succesful -> return from Monad / pure from Applicative
-- repetitions - some and many from Alternative

-- select one character

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x : xs) -> [(x, xs)])

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

string :: String -> Parser String 
string [] = return []
string (c : cs) = do char c
                     string cs
                     return (c : cs)

ident :: Parser String
ident = do c <- lower
           cs <- many alphanum
           return (c : cs)

nat :: Parser Int
nat = do ns <- some digit
         return (read ns)

int :: Parser Int
int = do char '-'
         ns <- nat
         return (-ns)
      <|> nat

space :: Parser ()
space = do many (sat isSpace)
           return ()

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

  {-
     Initial grammar:

     expr ::= expr + expr | expr * expr | ( expr ) | nat
     nat ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | ...

     Removing ambiguity and performing left-factorization,

     expr ::= term (+ expr | epsilon)
     term ::= factor (* term | epsilon)
     factor ::= ( expr ) | nat
     nat ::= 0 | 1 | 2 | ...
  -}

expr :: Parser Int
expr = do t <- term 
          do symbol "+"
             e <- expr
             return (t + e)
             <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
             <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            <|> natural

eval :: String -> Int
eval inp = case parse expr inp of
             [] -> error "Invalid input\n"
             [(v, [])] -> v
             [(_, out)] -> error $ "Unused input: " ++ out ++ "\n"