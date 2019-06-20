module Typeclasses.Extensions.List exposing (..)

import Typeclasses.Classes.Monoid as Monoid exposing (Monoid)
import Typeclasses.Classes.Comparison as Comparison exposing (Comparison)
import Typeclasses.Classes.Equality as Equality exposing (Equality)


fold : Monoid a -> List a -> a
fold monoid = List.foldl monoid.semigroup.prepend monoid.identity

foldMap : Monoid b -> (a -> b) -> List a -> b
foldMap monoidOfB aToB = List.foldl (monoidOfB.semigroup.prepend << aToB) monoidOfB.identity

sort : Comparison a -> List a -> List a
sort comparison = List.sortWith comparison.compare

sortBy : Comparison b -> (a -> b) -> List a -> List a
sortBy comparisonOfB aToB = List.sortWith (\ la ra -> comparisonOfB.compare (aToB la) (aToB ra))

member : Equality a -> a -> List a -> Bool
member equality a =
  let
    loop list = case list of
      head :: tail -> if equality.eq a head
        then True
        else loop tail
      _ -> False
    in loop

unique : Equality a -> List a -> List a
unique equality = List.foldr (\ a newList -> if member equality a newList then newList else a :: newList) []
