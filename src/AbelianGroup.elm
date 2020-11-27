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

import Basics
import CommutativeMonoid


{-| Explicit typeclass which implements group operations for type `a` when the operation is commutative.
-}
type AbelianGroup a
    = AbelianGroup
        { monoid : CommutativeMonoid.CommutativeMonoid a
        , inverse : a -> a
        }


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : AbelianGroup number
numberSum =
    { monoid = CommutativeMonoid.numberSum
    , inverse = \number -> -number
    }
        |> AbelianGroup


{-| Construct trivial group
-}
trivialGroup : AbelianGroup ()
trivialGroup =
    { monoid = CommutativeMonoid.unit
    , inverse = \() -> ()
    }
        |> AbelianGroup


{-| Construct exclusive Or
-}
exclusiveOr : AbelianGroup Bool
exclusiveOr =
    { monoid = CommutativeMonoid.exclusiveOr
    , inverse = Basics.identity
    }
        |> AbelianGroup


{-| Instance for modularArithmetic
-}
modularArithmetic : Int -> AbelianGroup Int
modularArithmetic divisor =
    { monoid = CommutativeMonoid.modularArithmetic divisor
    , inverse = \a -> divisor - a
    }
        |> AbelianGroup
