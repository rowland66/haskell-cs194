{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Expr
  (
  ExprT,
  Expr(lit,var,add,mul,eval),
  MinMax,
  Mod7,
  BoolProgram
  ) where

import StackVM

data ExprT x = Lit x
           | Var String
           | Add (ExprT x) (ExprT x)
           | Mul (ExprT x) (ExprT x)
  deriving (Show, Eq)

class Expr a where
  lit :: Integer -> ExprT a
  var :: [(String, Integer)] -> String -> ExprT a
  add :: ExprT a -> ExprT a -> ExprT a
  mul :: ExprT a -> ExprT a -> ExprT a
  eval :: ExprT a -> a

instance Expr Integer where
  lit x = Lit x
  var map x = Lit (maybe 0 id (lookup x map))
  add x y = Expr.Add x y
  mul x y = Expr.Mul x y
  eval (Lit x) = x
  eval (Expr.Add x y) = (eval x) + (eval y)
  eval (Expr.Mul x y) = (eval x) * (eval y)

instance Expr Bool where
  lit x | x <= 0 = Lit False
  lit x | x > 0 = Lit True
  var map x = Lit True
  add x y = Expr.Add x y
  mul x y = Expr.Mul x y
  eval (Lit x) = x
  eval (Expr.Add x y) = ((eval x) || (eval y))
  eval (Expr.Mul x y) = ((eval x) && (eval y))

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)

instance Expr MinMax where
  lit x = Lit (MinMax x)
  var map x = Lit (MinMax 1)
  add x y = Expr.Add x y
  mul x y = Expr.Mul x y
  eval (Lit x) = x
  eval (Expr.Add x y) = max (eval x) (eval y)
  eval (Expr.Mul x y) = min (eval x) (eval y)

newtype Mod7 = Mod7 Integer deriving (Eq, Num, Ord, Real, Enum, Integral, Show)

instance Expr Mod7 where
  lit x | x < 7 && x > -7 = Lit (Mod7 x)
  lit x | otherwise = Lit (Mod7 0)
  var map x = Lit (Mod7 1)
  add x y = Expr.Add x y
  mul x y = Expr.Mul x y
  eval (Lit x) = x
  eval (Expr.Add x y) = mod ((eval x) + (eval y)) 7
  eval (Expr.Mul x y) = mod ((eval x) * (eval y)) 7

instance Expr Program where
  lit x = Lit [PushI x]
  var map x = Lit [PushI (maybe 0 id (lookup x map))]
  add x y = Expr.Add x y
  mul x y = Expr.Mul x y
  eval (Lit x) = x
  eval (Expr.Add x y) = (eval x) ++ (eval y) ++ [StackVM.Add]
  eval (Expr.Mul x y) = (eval x) ++ (eval y) ++ [StackVM.Mul]

newtype BoolProgram = BoolProgram Program deriving (Show)

fromBoolProgram :: BoolProgram -> Program
fromBoolProgram (BoolProgram p) = p

instance Expr BoolProgram where
  lit x | x <= 0 = Lit (BoolProgram [PushB False])
  lit x          = Lit (BoolProgram [PushB True])
  var map x = Lit(BoolProgram [PushB True])
  add x y = Expr.Add x y
  mul x y = Expr.Mul x y
  eval (Lit x) = x
  eval (Expr.Add x y) = BoolProgram $ (fromBoolProgram (eval x)) ++ (fromBoolProgram (eval y)) ++ [StackVM.Or]
  eval (Expr.Mul x y) = BoolProgram $ (fromBoolProgram (eval x)) ++ (fromBoolProgram (eval y)) ++ [StackVM.And]