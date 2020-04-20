module Typeclasses.Classes.AbelianGroup exposing
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
import Typeclasses.Classes.CommutativeMonoid


type AbelianGroup a
    = AbelianGroup
        { monoid : Typeclasses.Classes.CommutativeMonoid.CommutativeMonoid a
        , inverse : a -> a
        }


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : AbelianGroup number
numberSum =
    { monoid = Typeclasses.Classes.CommutativeMonoid.numberSum
    , inverse = \number -> -number
    }
        |> AbelianGroup


{-| Construct trivial group
-}
trivialGroup : AbelianGroup ()
trivialGroup =
    { monoid = Typeclasses.Classes.CommutativeMonoid.unit
    , inverse = \() -> ()
    }
        |> AbelianGroup


{-| Construct exclusive Or
-}
exclusiveOr : AbelianGroup Bool
exclusiveOr =
    { monoid = Typeclasses.Classes.CommutativeMonoid.exclusiveOr
    , inverse = Basics.identity
    }
        |> AbelianGroup


{-| Instance for modularArithmetic
-}
modularArithmetic : Int -> AbelianGroup Int
modularArithmetic divisor =
    { monoid = Typeclasses.Classes.CommutativeMonoid.modularArithmetic divisor
    , inverse = \a -> divisor - a
    }
        |> AbelianGroup
