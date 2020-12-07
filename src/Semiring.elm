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
        sumMonoid =
            CommutativeMonoid.numberSum
    in
    { addition = sumMonoid
    , multiplication = Monoid.numberProduct
    }


{-| Construct trivial ring
-}
trivialRing : Semiring ()
trivialRing =
    { addition = CommutativeMonoid.unit
    , multiplication = Monoid.unit
    }


{-| Construct exclusive all ring
-}
exclusiveOrRing : Semiring Bool
exclusiveOrRing =
    let
        exclusiveOrMonoid =
            CommutativeMonoid.exclusiveOr
    in
    { addition = exclusiveOrMonoid
    , multiplication = Monoid.exclusiveOr
    }
