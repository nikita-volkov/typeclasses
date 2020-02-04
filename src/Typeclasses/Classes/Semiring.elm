module Typeclasses.Classes.Semiring exposing (Semiring)

{-| Semiring typeclass definition and its instances for basic types.


# Definition

@docs Semiring

-}

import Typeclasses.Classes.Monoid


{-| Explicit typeclass which implements group operations for type `a`.
-}
type alias Semiring a =
    { addition : Typeclasses.Classes.Monoid.Monoid a
    , multiplication : Typeclasses.Classes.Monoid.Monoid a
    }
