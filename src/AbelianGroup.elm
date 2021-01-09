module AbelianGroup exposing
    ( AbelianGroup(..)
    , numberSum, trivial, exclusiveOr, modularArithmetic
    )

{-| Abelian Group typeclass definition and its instances for basic types.


# Definition

@docs AbelianGroup

#Instances

@docs numberSum, trivial, exclusiveOr, modularArithmetic

-}

import Group


{-| Explicit typeclass which implements group operations for type `a` when the operation is commutative.
-}
type AbelianGroup a
    = AbelianGroup (Group.Group a)


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : AbelianGroup number
numberSum =
    AbelianGroup Group.numberSum


{-| Construct trivial group
-}
trivial : AbelianGroup ()
trivial =
    AbelianGroup Group.trivial


{-| Construct exclusive Or
-}
exclusiveOr : AbelianGroup Bool
exclusiveOr =
    AbelianGroup Group.exclusiveOr


{-| Instance for modularArithmetic
-}
modularArithmetic : Int -> AbelianGroup Int
modularArithmetic divisor =
    Group.modularArithmetic divisor
        |> AbelianGroup
