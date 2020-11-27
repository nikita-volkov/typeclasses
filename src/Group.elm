module Group exposing
    ( Group
    , numberSum, trivialGroup, exclusiveOr, modularArithmetic
    )

{-| Group typeclass definition and its instances for basic types.


# Definition

@docs Group

#Instances

@docs numberSum, trivialGroup, exclusiveOr, modularArithmetic

-}

import AbelianGroup
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
    let
        (AbelianGroup.AbelianGroup group) =
            AbelianGroup.numberSum
    in
    { monoid = Monoid.numberSum
    , inverse = group.inverse
    }


{-| Construct trivial group
-}
trivialGroup : Group ()
trivialGroup =
    let
        (AbelianGroup.AbelianGroup group) =
            AbelianGroup.trivialGroup
    in
    { monoid = Monoid.unit
    , inverse = group.inverse
    }


{-| Construct exclusive Or
-}
exclusiveOr : Group Bool
exclusiveOr =
    let
        (AbelianGroup.AbelianGroup group) =
            AbelianGroup.exclusiveOr
    in
    { monoid = Monoid.exclusiveOr
    , inverse = group.inverse
    }


{-| Instance for modularArithmetic
-}
modularArithmetic : Int -> Group Int
modularArithmetic divisor =
    let
        (AbelianGroup.AbelianGroup group) =
            AbelianGroup.modularArithmetic divisor
    in
    { monoid = Monoid.modularArithmetic divisor
    , inverse = group.inverse
    }
