module Semiring exposing
    ( Semiring
    , numberRing, trivialRing, exclusiveOrRing
    )

{-| Semiring typeclass definition and its instances for basic types.


# Definition

@docs Semiring

#Instances

@docs numberRing, trivialRing, exclusiveOrRing

-}

import AbelianGroup
import CommutativeMonoid
import Monoid
import Ring


{-| Explicit typeclass which implements group operations for type `a`.
-}
type alias Semiring a =
    { addition : CommutativeMonoid.CommutativeMonoid a
    , multiplication : Monoid.Monoid a
    }


{-| Construct real number ring
-}
numberRing : Semiring number
numberRing =
    let
        ring =
            Ring.numberRing

        (AbelianGroup.AbelianGroup group) =
            ring.addition
    in
    { addition = group.monoid
    , multiplication = Monoid.numberProduct
    }


{-| Construct trivial ring
-}
trivialRing : Semiring ()
trivialRing =
    let
        ring =
            Ring.trivialRing

        (AbelianGroup.AbelianGroup group) =
            ring.addition
    in
    { addition = group.monoid
    , multiplication = Monoid.unit
    }


{-| Construct exclusive all ring
-}
exclusiveOrRing : Semiring Bool
exclusiveOrRing =
    let
        ring =
            Ring.exclusiveOrRing

        (AbelianGroup.AbelianGroup group) =
            ring.addition
    in
    { addition = group.monoid
    , multiplication = Monoid.exclusiveOr
    }
