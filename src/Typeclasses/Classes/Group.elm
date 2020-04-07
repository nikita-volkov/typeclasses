module Typeclasses.Classes.Group exposing (Group)

{-| Group typeclass definition and its instances for basic types.


# Definition

@docs Group

-}

import Typeclasses.Classes.Monoid exposing (Monoid)


{-| Explicit typeclass which implements group operations for type `a`.
-}
type alias Group a =
    { monoid : Monoid a
    , inverse : a -> a
    }
