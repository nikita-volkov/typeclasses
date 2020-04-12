module Typeclasses.Extensions.Dict exposing (foldMap)

{-| Extensions to the dict API, which utilise typeclasses.

@docs foldMap

-}

import Dict exposing (Dict)
import Typeclasses.Classes.Monoid as Monoid exposing (Monoid)


{-| _O(n)_. Map each entry of dict to a type which has an instance of `Monoid`,
and combine the results.
-}
foldMap : Monoid c -> (a -> b -> c) -> Dict a b -> c
foldMap monoid mapping =
    Dict.foldl (\a b -> monoid.semigroup (mapping a b)) monoid.identity
