{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module JoinList where

import Sized
import Scrabble
import Data.Foldable

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ Empty = Empty
Empty +++ jl = jl
jl +++ Empty = jl
jl1 +++ jl2 = Append m' jl1 jl2
              where m' = (tag jl1) <> (tag jl2)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

--instance Foldable (JoinList (Score, Size)) where
--foldMap f Empty = ()
--foldMap f (Single m a) = f a
--foldMap f (Append m jl1 jl2) = (JoinList.foldMap f jl1) <> (JoinList.foldMap f jl2)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ ix _ | ix < 0 = Nothing
indexJ ix (Single b a) | ix == 0 = Just (a)
indexJ _ (Single b a) = Nothing
indexJ ix (Append b jl1 jl2) | ix >= (getSize (size b)) = Nothing
indexJ ix (Append b jl1 jl2) = if (ix < jl1Size) then
                                 indexJ ix jl1
                               else
                                 indexJ (ix - jl1Size) jl2
                               where
                                 jl1Size = getSize . size . tag $ jl1

replaceJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a -> JoinList b a
replaceJ _ Empty jl = Empty
replaceJ 0 jl@(Single _ _) jlReplace = jlReplace
replaceJ _ jl@(Single _ _) _ = jl
replaceJ ix jl@(Append b jl1 jl2) _ | ix >= (getSize (size b)) = jl
replaceJ ix (Append b jl1 jl2) jlReplace = if (ix < jl1Size) then
                                             (replaceJ ix jl1 jlReplace) +++ jl2
                                           else
                                             jl1 +++ (replaceJ (ix - jl1Size) jl2 jlReplace)
                                           where
                                             jl1Size = getSize . size . tag $ jl1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ ix jl | ix <= 0 = jl
dropJ _ Empty = Empty
dropJ _ (Single b a) = Empty
dropJ ix (Append b jl1 jl2) | (getSize (size b)) <= ix = Empty
dropJ ix (Append b jl1 jl2) =
  let dropResult = dropJ ix jl1
      jl1Size = getSize . size . tag $ jl1
  in
  case dropResult of
    Empty -> if ix > jl1Size then dropResult +++ (dropJ (ix -jl1Size) jl2) else dropResult +++ jl2
    otherwise -> dropResult +++ jl2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ ix _ | ix <= 0 = Empty
takeJ _ Empty = Empty
takeJ _ jl@(Single _ _) = jl
takeJ ix jl@(Append b _ _) | (getSize (size b)) <= ix = jl
takeJ ix jl@(Append _ jl1 jl2) =
  let takeResult = takeJ ix jl1
      takeResultSize = getSize . size . tag $ takeResult
  in
  if (ix - takeResultSize) == 0 then
    takeResult
  else
    takeResult +++ (takeJ (ix - takeResultSize) jl2)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s