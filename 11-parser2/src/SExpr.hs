{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char (isSpace, isAlpha, isAlphaNum)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = many_p
  where
    many_p = some_p <|> pure []
    some_p = (:) <$> p <*> many_p

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = some_p
  where
    many_p = some_p <|> pure []
    some_p = (:) <$> p <*> many_p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = someAlpha_p
  where
    someAlpha_p = (:) <$> alpha_parser <*> manyAlphaNum_p
    manyAlphaNum_p = someAlphaNum_p <|> pure []
    someAlphaNum_p = (:) <$> alphaNum_parser <*> manyAlphaNum_p
    alpha_parser = (satisfy isAlpha)
    alphaNum_parser = (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseNumericAtom :: Parser Atom
parseNumericAtom = (\x -> N x) <$> posInt

parseIdentAtom :: Parser Atom
parseIdentAtom = (\x -> I x) <$> ident

parseAtom :: Parser SExpr
parseAtom = (\x -> A x) <$> (spaces *> (parseNumericAtom <|> parseIdentAtom))

parseComb :: Parser SExpr
parseComb = (\x -> Comb x) <$> ((spaces *> (char '(')) *> (spaces *> someSExpr_p <* spaces) <* (char ')'))
  where
    manySExpr_p = someSExpr_p <|> pure []
    someSExpr_p = (\x xs -> x : xs) <$> (parseSExpr <* spaces) <*> manySExpr_p

parseSExpr :: Parser SExpr
parseSExpr = parseAtom <|> parseComb