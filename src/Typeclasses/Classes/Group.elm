module Typeclasses.Classes.Group exposing
    ( Group
    , floatProduct, numberSum, trivialGroup, exclusiveOr, modularArithmetic
    )

{-| Group typeclass definition and its instances for basic types.


# Definition

@docs Group

#Instances

@docs floatProduct, numberSum, trivialGroup, exclusiveOr, modularArithmetic

-}

import Basics
import Typeclasses.Classes.Monoid exposing (Monoid)


{-| Explicit typeclass which implements group operations for type `a`.
-}
type alias Group a =
    { monoid : Monoid a
    , inverse : a -> a
    }


type AbelianGroup a
    = AbelianGroup
        { monoid : Typeclasses.Classes.Monoid.CommutativeMonoid a
        , inverse : a -> a
        }


{-| Construct an instance for Float.
Implements multiplication.
-}
floatProduct : AbelianGroup Float
floatProduct =
    { monoid = Typeclasses.Classes.Monoid.numberProduct
    , inverse = \number -> 1 / number
    }
        |> AbelianGroup


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : Group number
numberSum =
    { monoid = Typeclasses.Classes.Monoid.numberSum
    , inverse = \number -> -number
    }


{-| Construct trivial group
-}
trivialGroup : Group ()
trivialGroup =
    { monoid = Typeclasses.Classes.Monoid.unit
    , inverse = \() -> ()
    }


{-| Construct exclusive Or
-}
exclusiveOr : Group Bool
exclusiveOr =
    { monoid = Typeclasses.Classes.Monoid.exclusiveOr
    , inverse = Basics.identity
    }


{-| Instance for modularArithmetic
-}
modularArithmetic : Int -> Group Int
modularArithmetic divisor =
    { monoid = Typeclasses.Classes.Monoid.modularArithmetic divisor
    , inverse = \a -> divisor - a
    }
