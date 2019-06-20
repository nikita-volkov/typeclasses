module Typeclasses.Classes.Comparison exposing (..)

import Typeclasses.Classes.Equality as Equality exposing (Equality)
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

equalityAndLt : Equality a -> (a -> a -> Bool) -> Comparison a
equalityAndLt equality lt_ =
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

{-| Map over the owner type of an instance to produce a new instance.

You need to provide both a covariant and a contravariant mapping
(i.e., `(a -> b)` and `(b -> a)`).
-}
map : (a -> b) -> (b -> a) -> Comparison a -> Comparison b
map aToB bToA comparisonOfA =
  {
    equality = Equality.map bToA comparisonOfA.equality
    ,
    compare = \ lb rb -> comparisonOfA.compare (bToA lb) (bToA rb)
    ,
    lt = \ lb rb -> comparisonOfA.lt (bToA lb) (bToA rb)
    ,
    le = \ lb rb -> comparisonOfA.le (bToA lb) (bToA rb)
    ,
    gt = \ lb rb -> comparisonOfA.gt (bToA lb) (bToA rb)
    ,
    ge = \ lb rb -> comparisonOfA.ge (bToA lb) (bToA rb)
    ,
    min = \ lb rb -> aToB (comparisonOfA.min (bToA lb) (bToA rb))
    ,
    max = \ lb rb -> aToB (comparisonOfA.max (bToA lb) (bToA rb))
  }
