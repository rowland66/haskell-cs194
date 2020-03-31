{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Data.Monoid
import Buffer
import JoinList
import Scrabble
import Sized

instance Buffer (JoinList (Score, Size) String) where
  toString (Empty) = ""
  toString (Single b a) = a
  toString (Append b jl1 jl2) = (toString jl1) ++ (toString jl2)
  fromString s = foldr (\l jl -> ((Single ((scoreString l), Size 1) l)) +++ jl) Empty (lines s)
  line n b = indexJ n b
  replaceLine n l jl = replaceJ n jl (Single ((scoreString l), Size 1) l)
  numLines = getSize . size . tag
  value = getScore . fst . tag
