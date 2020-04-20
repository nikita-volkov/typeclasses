module Typeclasses.Classes.Group exposing
    ( Group
    , exclusiveOr, modularArithmetic, numberSum, trivialGroup
    )

{-| Group typeclass definition and its instances for basic types.


# Definition

@docs Group

-}

import Typeclasses.Classes.AbelianGroup
import Typeclasses.Classes.CommutativeMonoid
import Typeclasses.Classes.CommutativeSemigroup
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
    let
        (Typeclasses.Classes.AbelianGroup.AbelianGroup group) =
            Typeclasses.Classes.AbelianGroup.numberSum
    in
    { monoid = Typeclasses.Classes.Monoid.numberSum
    , inverse = group.inverse
    }


{-| Construct trivial group
-}
trivialGroup : Group ()
trivialGroup =
    let
        (Typeclasses.Classes.AbelianGroup.AbelianGroup group) =
            Typeclasses.Classes.AbelianGroup.trivialGroup
    in
    { monoid = Typeclasses.Classes.Monoid.unit
    , inverse = group.inverse
    }


{-| Construct exclusive Or
-}
exclusiveOr : Group Bool
exclusiveOr =
    let
        (Typeclasses.Classes.AbelianGroup.AbelianGroup group) =
            Typeclasses.Classes.AbelianGroup.exclusiveOr
    in
    { monoid = Typeclasses.Classes.Monoid.exclusiveOr
    , inverse = group.inverse
    }


{-| Instance for modularArithmetic
-}
modularArithmetic : Int -> Group Int
modularArithmetic divisor =
    let
        (Typeclasses.Classes.AbelianGroup.AbelianGroup group) =
            Typeclasses.Classes.AbelianGroup.modularArithmetic divisor
    in
    { monoid = Typeclasses.Classes.Monoid.modularArithmetic divisor
    , inverse = group.inverse
    }
