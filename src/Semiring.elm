module Semiring exposing
    ( Semiring
    , number, trivial, exclusiveOr
    )

{-| Semiring typeclass definition and its instances for basic types.


# Definition

@docs Semiring

#Instances

@docs number, trivial, exclusiveOr

-}

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
number : Semiring number
number =
    let
        sumMonoid =
            CommutativeMonoid.numberSum
    in
    { addition = sumMonoid
    , multiplication = Monoid.numberProduct
    }


{-| Construct trivial ring
-}
trivial : Semiring ()
trivial =
    { addition = CommutativeMonoid.unit
    , multiplication = Monoid.unit
    }


{-| Construct exclusive all ring
-}
exclusiveOr : Semiring Bool
exclusiveOr =
    let
        exclusiveOrMonoid =
            CommutativeMonoid.exclusiveOr
    in
    { addition = exclusiveOrMonoid
    , multiplication = Monoid.exclusiveOr
    }
