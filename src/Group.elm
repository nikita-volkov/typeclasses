module Group exposing
    ( Group
    , numberSum, trivialGroup, exclusiveOr, modularArithmetic
    , floatProduct
    )

{-| Group typeclass definition and its instances for basic types.


# Definition

@docs Group

#Instances

@docs numberSum, trivialGroup, exclusiveOr, modularArithmetic

-}

import CommutativeMonoid
import CommutativeSemigroup
import Monoid exposing (Monoid)


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
    { monoid = Monoid.numberSum
    , inverse = \number -> -number
    }


{-| Implements product.
-}
floatProduct : Group Float
floatProduct =
    { monoid = Monoid.numberSum
    , inverse = \number -> 1 / number
    }


{-| Construct trivial group
-}
trivialGroup : Group ()
trivialGroup =
    { monoid = Monoid.unit
    , inverse = \() -> ()
    }


{-| Construct exclusive Or
-}
exclusiveOr : Group Bool
exclusiveOr =
    { monoid = Monoid.exclusiveOr
    , inverse = Basics.identity
    }


{-| Instance for modularArithmetic
-}
modularArithmetic : Int -> Group Int
modularArithmetic divisor =
    { monoid = Monoid.modularArithmetic divisor
    , inverse = \a -> divisor - a
    }
