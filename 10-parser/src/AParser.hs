{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Maybe

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

parseAlphaString :: Parser String
parseAlphaString = Parser f
  where
    consumeStr :: String -> (String, String)
    consumeStr r@(x:xs)
      | isAlpha x = let result = consumeStr xs
                    in (x:(fst result), snd result)
      | otherwise = ([], r)
    consumeStr [] = ([], [])
    f :: String -> Maybe(String, String)
    f xs
      | xs == [] = Nothing
      | null ns = Nothing
      | otherwise = Just (ns, rest)
      where (ns, rest) = consumeStr xs

parseNumericString :: Parser String
parseNumericString = Parser f
  where
    consumeStr :: String -> (String, String)
    consumeStr r@(x:xs)
      | isDigit x = let result = consumeStr xs
                    in (x:(fst result), snd result)
      | otherwise = ([], r)
    consumeStr [] = ([], [])
    f :: String -> Maybe(String, String)
    f xs
      | xs == [] = Nothing
      | null ns = Nothing
      | otherwise = Just (ns, rest)
      where (ns, rest) = consumeStr xs

instance Functor Parser where
  fmap f (Parser a) = Parser (\s -> fmap (first f) (a s))

instance Applicative Parser where
  pure a = Parser (\s -> Just (a,s))
  (Parser f1) <*> a = Parser (\s -> join (fmap (\(rf, rem) -> runParser (rf <$> a) rem) (f1 s)))

instance Alternative Parser where
  empty = Parser (\s -> Nothing)
  (Parser p1) <|> (Parser p2) = Parser (\s -> if (isNothing (p1 s)) then (p2 s) else p1 s)

type Name = String
data Employee = Emp { name :: Name, phone :: String }
  deriving (Show)

parseName  :: Parser Name
parseName = parseAlphaString

parsePhone :: Parser String
parsePhone = parseNumericString

abParser :: Parser (Char, Char)
abParser = (\a b -> (a,b)) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\a b -> ()) <$> (char 'a') <*> (char 'b')

intPair :: Parser [Integer]
intPair = (\n1 s n2 -> n1:n2:[]) <$> posInt <*> (char ' ') <*> posInt

intOrUppercase :: Parser ()
intOrUppercase = (\c -> ()) <$> ((satisfy isUpper) <|> (satisfy isDigit))
