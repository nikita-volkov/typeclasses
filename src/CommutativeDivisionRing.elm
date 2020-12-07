module CommutativeDivisionRing exposing
    ( CommutativeDivisionRing(..)
    , floatCommutativeDivisionRing, trivialCommutativeDivisionRing
    )

{-| Commutative Divison Ring typeclass definition and its instances for basic types.


# Definition

@docs CommutativeDivisionRing

#Instances

@docs floatCommutativeDivisionRing, trivialCommutativeDivisionRing

-}

import AbelianGroup
import DivisionRing exposing (DivisionRing)
import Group
import Monoid


{-| Explicit typeclass which implements division ring operations for type `a`.
-}
type CommutativeDivisionRing a
    = CommutativeDivisionRing (DivisionRing.DivisionRing a)


{-| Construct real number division ring
-}
floatCommutativeDivisionRing : CommutativeDivisionRing Float
floatCommutativeDivisionRing =
    CommutativeDivisionRing
        DivisionRing.floatDivisionRing


{-| Construct trivial Division ring
-}
trivialCommutativeDivisionRing : CommutativeDivisionRing ()
trivialCommutativeDivisionRing =
    CommutativeDivisionRing
        DivisionRing.trivialDivisionRing
