module Typeclasses.Classes.Semiring exposing
    ( Semiring
    , exclusiveOrRing, numberRing, trivialRing
    )

{-| Semiring typeclass definition and its instances for basic types.


# Definition

@docs Semiring

-}

import Typeclasses.Classes.AbelianGroup
import Typeclasses.Classes.CommutativeMonoid
import Typeclasses.Classes.Monoid
import Typeclasses.Classes.Ring


{-| Explicit typeclass which implements group operations for type `a`.
-}
type alias Semiring a =
    { addition : Typeclasses.Classes.CommutativeMonoid.CommutativeMonoid a
    , multiplication : Typeclasses.Classes.Monoid.Monoid a
    }


{-| Construct real number ring
-}
numberRing : Semiring number
numberRing =
    let
        ring =
            Typeclasses.Classes.Ring.numberRing

        (Typeclasses.Classes.AbelianGroup.AbelianGroup group) =
            ring.addition
    in
    { addition = group.monoid
    , multiplication = Typeclasses.Classes.Monoid.numberProduct
    }


{-| Construct trivial ring
-}
trivialRing : Semiring ()
trivialRing =
    let
        ring =
            Typeclasses.Classes.Ring.trivialRing

        (Typeclasses.Classes.AbelianGroup.AbelianGroup group) =
            ring.addition
    in
    { addition = group.monoid
    , multiplication = Typeclasses.Classes.Monoid.unit
    }


{-| Construct exclusive all ring
-}
exclusiveOrRing : Semiring Bool
exclusiveOrRing =
    let
        ring =
            Typeclasses.Classes.Ring.exclusiveOrRing

        (Typeclasses.Classes.AbelianGroup.AbelianGroup group) =
            ring.addition
    in
    { addition = group.monoid
    , multiplication = Typeclasses.Classes.Monoid.exclusiveOr
    }
