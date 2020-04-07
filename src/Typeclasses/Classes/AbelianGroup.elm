module Typeclasses.Classes.AbelianGroup exposing
    ( floatProduct, numberSum, trivialGroup, exclusiveOr, modularArithmetic
    , AbelianGroup(..)
    )

{-| Group typeclass definition and its instances for basic types.


# Definition

@docs Group

#Instances

@docs floatProduct, numberSum, trivialGroup, exclusiveOr, modularArithmetic

-}

import Basics
import Typeclasses.Classes.Monoid


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
numberSum : AbelianGroup number
numberSum =
    { monoid = Typeclasses.Classes.Monoid.numberSum
    , inverse = \number -> -number
    }
        |> AbelianGroup


{-| Construct trivial group
-}
trivialGroup : AbelianGroup ()
trivialGroup =
    { monoid = Typeclasses.Classes.Monoid.unit
    , inverse = \() -> ()
    }
        |> AbelianGroup


{-| Construct exclusive Or
-}
exclusiveOr : AbelianGroup Bool
exclusiveOr =
    { monoid = Typeclasses.Classes.Monoid.exclusiveOr
    , inverse = Basics.identity
    }
        |> AbelianGroup


{-| Instance for modularArithmetic
-}
modularArithmetic : Int -> AbelianGroup Int
modularArithmetic divisor =
    { monoid = Typeclasses.Classes.Monoid.modularArithmetic divisor
    , inverse = \a -> divisor - a
    }
        |> AbelianGroup
