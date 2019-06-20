module Typeclasses.Comparison exposing (..)

import Typeclasses.Equality as Equality exposing (Equality)
import Either exposing (Either(..))

{-| Explicit typeclass which implements comparison operations for type `a`. -}
type alias Comparison a =
  {
    equality : Equality a,
    compare : a -> a -> Order,
    lt : a -> a -> Bool,
    le : a -> a -> Bool,
    gt : a -> a -> Bool,
    ge : a -> a -> Bool,
    min : a -> a -> a,
    max : a -> a -> a
  }

compare : (a -> a -> Order) -> Comparison a
compare compare_ =
  {
    equality = Equality.compare compare_
    ,
    compare = compare_
    ,
    lt = \ l r -> case compare_ l r of
      LT -> True
      _ -> False
    ,
    le = \ l r -> case compare_ l r of
      LT -> True
      EQ -> True
      _ -> False
    ,
    gt = \ l r -> case compare_ l r of
      GT -> True
      _ -> False
    ,
    ge = \ l r -> case compare_ l r of
      GT -> True
      EQ -> True
      _ -> False
    ,
    min = \ l r -> case compare_ l r of
      LT -> l
      _ -> r
    ,
    max = \ l r -> case compare_ l r of
      LT -> r
      _ -> l
  }

lt : Equality a -> (a -> a -> Bool) -> Comparison a
lt equality lt_ =
  {
    equality = equality
    ,
    compare = \ l r -> if lt_ l r
      then LT
      else if equality.eq l r
        then EQ
        else GT
    ,
    lt = lt_
    ,
    le = \ l r -> if lt_ l r
      then True
      else if equality.eq l r
        then True
        else False
    ,
    gt = \ l r -> if lt_ l r
      then False
      else if equality.eq l r
        then False
        else True
    ,
    ge = \ l r -> if lt_ l r
      then False
      else if equality.eq l r
        then True
        else True
    ,
    min = \ l r -> if lt_ l r then l else r
    ,
    max = \ l r -> if lt_ l r then r else l
  }

comparable : Comparison comparable
comparable =
  {
    equality = Equality.comparable,
    compare = Basics.compare,
    lt = (<),
    le = (<=),
    gt = (>),
    ge = (>=),
    min = min,
    max = max
  }

int : Comparison Int
int = comparable

float : Comparison Float
float = comparable
