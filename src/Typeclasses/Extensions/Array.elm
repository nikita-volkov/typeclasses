module Typeclasses.Extensions.Array exposing (fold, foldMap, member)
{-| Extensions to the array API, which utilise typeclasses.

@docs fold, foldMap, member

-}

import Array exposing (Array)
import Typeclasses.Classes.Monoid as Monoid exposing (Monoid)
import Typeclasses.Classes.Comparison as Comparison exposing (Comparison)
import Typeclasses.Classes.Equality as Equality exposing (Equality)

{-| *O(n)*. Combine the elements of array using a `Monoid` instance. -}
fold : Monoid a -> Array a -> a
fold monoid = foldMap monoid identity

{-| *O(n)*. Map each element of the array to a type which has an instance of `Monoid`,
and combine the results. -}
foldMap : Monoid b -> (a -> b) -> Array a -> b
foldMap monoidOfB aToB = Array.foldl (monoidOfB.semigroup.prepend << aToB) monoidOfB.identity

{-| *O(n)*. Check whether a value is a member of an array based on the value's `Equality` instance. -}
member : Equality a -> a -> Array a -> Bool
member equality a array =
  let
    loop index = case Array.get index array of
      Just element -> if equality.eq a element
        then True
        else loop (index + 1)
      _ -> False
    in loop 0
