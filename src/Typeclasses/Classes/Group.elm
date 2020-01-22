module Typeclasses.Classes.Group exposing
    ( Group
    , numberSum
    )

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


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : Group number
numberSum =
    { monoid = Typeclasses.Classes.Monoid.numberSum
    , inverse = \number -> -number
    }
