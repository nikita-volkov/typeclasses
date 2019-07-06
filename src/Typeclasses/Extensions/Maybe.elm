module Typeclasses.Extensions.Maybe exposing (fold, foldMap)
{-| Extensions to the Maybe API, which utilise typeclasses.

@docs fold, foldMap

-}

import Typeclasses.Classes.Monoid as Monoid exposing (Monoid)
import Typeclasses.Classes.Comparison as Comparison exposing (Comparison)
import Typeclasses.Classes.Equality as Equality exposing (Equality)

{-|
Extract a value or use the Monoid's identity.
-}
fold : Monoid a -> Maybe a -> a
fold monoid = Maybe.withDefault monoid.identity

{-|
Map the element of Maybe to a type which has an instance of `Monoid`,
or use its identity, when Maybe is Nothing.
-}
foldMap : Monoid b -> (a -> b) -> Maybe a -> b
foldMap monoidOfB aToB maybe = case maybe of
  Just a -> aToB a
  Nothing -> monoidOfB.identity
