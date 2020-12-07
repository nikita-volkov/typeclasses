module DivisionRing exposing (..)

{-| Divison Ring typeclass definition and its instances for basic types.


# Definition

@docs Division Ring

#Instances

@docs floatDivisionRing, trivialDivisionRing

-}

import AbelianGroup
import Group
import Monoid


{-| Explicit typeclass which implements ring operations for type `a`.
-}
type alias DivisionRing a =
    { addition : AbelianGroup.AbelianGroup a
    , multiplication : Group.Group a
    }


{-| Construct real number division ring
-}
floatDivisionRing : DivisionRing Float
floatDivisionRing =
    { addition = AbelianGroup.numberSum
    , multiplication = Group.floatProduct
    }


{-| Construct trivial Division ring
-}
trivialDivisionRing : DivisionRing ()
trivialDivisionRing =
    { addition = AbelianGroup.trivialGroup
    , multiplication = Group.trivialGroup
    }
