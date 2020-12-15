module AbelianGroup exposing
    ( AbelianGroup(..)
    , numberSum, trivialGroup, exclusiveOr, modularArithmetic
    )

{-| Abelian Group typeclass definition and its instances for basic types.


# Definition

@docs AbelianGroup

#Instances

@docs numberSum, trivialGroup, exclusiveOr, modularArithmetic

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
trivialGroup : AbelianGroup ()
trivialGroup =
    AbelianGroup Group.trivialGroup


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
