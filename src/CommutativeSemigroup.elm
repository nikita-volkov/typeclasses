module CommutativeSemigroup exposing
    ( CommutativeSemigroup(..)
    , numberProduct, intProduct, numberSum, intSum, setUnion, setIntersection, and, or, unit, xor, modularArithmetic
    )

{-| Commutative Semigroup typeclass definition and its instances for basic types.


# Definition

@docs CommutativeSemigroup


# Instances

@docs numberProduct, intProduct, numberSum, intSum, setUnion, setIntersection, and, or, unit, xor, modularArithmetic

-}

import Semigroup exposing (Semigroup)
import Set


{-| Explicit typeclass which implements semigroup operations for type `a` when the operation is commutative.
-}
type CommutativeSemigroup a
    = CommutativeSemigroup (Semigroup.Semigroup a)


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements multiplication.
-}
numberProduct : CommutativeSemigroup number
numberProduct =
    CommutativeSemigroup Semigroup.numberProduct


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : CommutativeSemigroup number
numberSum =
    CommutativeSemigroup Semigroup.numberSum


{-| Instance for integers under the multiplication operation.
-}
intProduct : CommutativeSemigroup Int
intProduct =
    numberProduct


{-| Instance for integers under the sum operation.
-}
intSum : CommutativeSemigroup Int
intSum =
    numberSum


{-| Instance for set under the union operation.
-}
setUnion : CommutativeSemigroup (Set.Set comparable)
setUnion =
    CommutativeSemigroup Semigroup.setUnion


{-| Instance for set under the intersection operation.
-}
setIntersection : CommutativeSemigroup (Set.Set comparable)
setIntersection =
    CommutativeSemigroup Semigroup.setIntersection


{-| Instance for and
-}
and : CommutativeSemigroup Bool
and =
    CommutativeSemigroup Semigroup.and


{-| Instance for or
-}
or : CommutativeSemigroup Bool
or =
    CommutativeSemigroup Semigroup.or


{-| Instance for trivial semigroup
-}
unit : CommutativeSemigroup ()
unit =
    CommutativeSemigroup Semigroup.unit


{-| Instance for xor
-}
xor : CommutativeSemigroup Bool
xor =
    CommutativeSemigroup Semigroup.xor


{-| Instance for modularArithmetic semigroup
-}
modularArithmetic : Int -> CommutativeSemigroup Int
modularArithmetic divisor =
    Semigroup.modularArithmetic divisor
        |> CommutativeSemigroup
