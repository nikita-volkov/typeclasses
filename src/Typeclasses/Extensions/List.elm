module Typeclasses.Extensions.List exposing (fold, foldMap, sort, sortBy, member, dedupe)
{-| Extensions to the list API, which utilise typeclasses.

@docs fold, foldMap, sort, sortBy, member, dedupe

-}

import Typeclasses.Classes.Monoid as Monoid exposing (Monoid)
import Typeclasses.Classes.Comparison as Comparison exposing (Comparison)
import Typeclasses.Classes.Equality as Equality exposing (Equality)

{-| *O(n)*. Combine the elements of list using a `Monoid` instance. -}
fold : Monoid a -> List a -> a
fold monoid = monoid.concat

{-| *O(n)*. Map each element of the list to a type which has an instance of `Monoid`,
and combine the results. -}
foldMap : Monoid b -> (a -> b) -> List a -> b
foldMap monoidOfB aToB = List.foldl (monoidOfB.semigroup.prepend << aToB) monoidOfB.identity

{-| Sort the elements using a `Comparison` instance. -}
sort : Comparison a -> List a -> List a
sort comparison = List.sortWith comparison.compare

{-| Sort the elements by mapping to a type which has an instance of `Comparison`. -}
sortBy : Comparison b -> (a -> b) -> List a -> List a
sortBy comparisonOfB aToB = List.sortWith (\ la ra -> comparisonOfB.compare (aToB la) (aToB ra))

{-| *O(n)*. Check whether a value is a member of a list based on the value's `Equality` instance. -}
member : Equality a -> a -> List a -> Bool
member equality a =
  let
    loop list = case list of
      head :: tail -> if equality.eq a head
        then True
        else loop tail
      _ -> False
    in loop

{-| *O(n^2)*. Deduplicate a list using an instance of `Equality`. -}
dedupe : Equality a -> List a -> List a
dedupe equality = List.foldr (\ a newList -> if member equality a newList then newList else a :: newList) []
