{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble (
  Score(..),
  getScore,
  scoreString )
where

import Data.Char

newtype Score = Score Int
  deriving  (Eq, Show, Ord, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty = Score 0

instance Semigroup Score where
  (<>) = (+)

score :: Char -> Score
score c | toUpper c == 'A' = 1
score c | toUpper c == 'B' = 3
score c | toUpper c == 'C' = 3
score c | toUpper c == 'D' = 2
score c | toUpper c == 'E' = 1
score c | toUpper c == 'F' = 4
score c | toUpper c == 'G' = 2
score c | toUpper c == 'H' = 4
score c | toUpper c == 'I' = 1
score c | toUpper c == 'J' = 8
score c | toUpper c == 'K' = 5
score c | toUpper c == 'L' = 1
score c | toUpper c == 'M' = 3
score c | toUpper c == 'N' = 1
score c | toUpper c == 'O' = 1
score c | toUpper c == 'P' = 3
score c | toUpper c == 'Q' = 10
score c | toUpper c == 'R' = 1
score c | toUpper c == 'S' = 1
score c | toUpper c == 'T' = 1
score c | toUpper c == 'U' = 1
score c | toUpper c == 'V' = 4
score c | toUpper c == 'W' = 4
score c | toUpper c == 'X' = 8
score c | toUpper c == 'Y' = 4
score c | toUpper c == 'Z' = 10
score _ = 0

scoreString :: String -> Score
scoreString = foldr (\c total -> (score c) + total) 0
