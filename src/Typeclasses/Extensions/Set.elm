module Typeclasses.Extensions.Set exposing (fold, foldMap)

{-| Extensions to the set API, which utilise typeclasses.

@docs fold, foldMap

-}

import Set exposing (Set)
import Typeclasses.Classes.Monoid as Monoid exposing (Monoid)


{-| _O(n)_. Combine the elements of set using a `Monoid` instance.
-}
fold : Monoid a -> Set a -> a
fold monoid =
    foldMap monoid identity


{-| _O(n)_. Map each element of the set to a type which has an instance of `Monoid`,
and combine the results.
-}
foldMap : Monoid b -> (a -> b) -> Set a -> b
foldMap monoidOfB aToB =
    Set.foldl (monoidOfB.semigroup << aToB) monoidOfB.identity
