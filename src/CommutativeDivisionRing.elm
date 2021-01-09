module CommutativeDivisionRing exposing
    ( CommutativeDivisionRing(..)
    , float, trivial
    )

{-| Commutative Divison Ring typeclass definition and its instances for basic types.


# Definition

@docs CommutativeDivisionRing

#Instances

@docs float, trivial

-}

import DivisionRing


{-| Explicit typeclass which implements division ring operations for type `a`.
-}
type CommutativeDivisionRing a
    = CommutativeDivisionRing (DivisionRing.DivisionRing a)


{-| Construct real number division ring
-}
float : CommutativeDivisionRing Float
float =
    CommutativeDivisionRing
        DivisionRing.floatDivisionRing


{-| Construct trivial Division ring
-}
trivial : CommutativeDivisionRing ()
trivial =
    CommutativeDivisionRing
        DivisionRing.trivialDivisionRing
