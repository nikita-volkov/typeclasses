module DivisionRing exposing
    ( DivisionRing
    , floatDivisionRing, trivialDivisionRing
    )

{-| Divison Ring typeclass definition and its instances for basic types.


# Definition

@docs DivisionRing

#Instances

@docs floatDivisionRing, trivialDivisionRing

-}

import AbelianGroup
import Group


{-| Explicit typeclass which implements division ring operations for type `a`.
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
