module Field exposing
    ( Field(..)
    , float, trivial
    )

{-| Field typeclass definition and its instances for basic types.


# Definition

@docs Field

#Instances

@docs float, trivial

-}

import CommutativeDivisionRing


{-| Explicit typeclass which implements group operations for type `a`.
-}
type Field a
    = Field (CommutativeDivisionRing.CommutativeDivisionRing a)


{-| Construct real number field
-}
float : Field Float
float =
    Field CommutativeDivisionRing.float


{-| Construct trivial field
-}
trivial : Field ()
trivial =
    Field CommutativeDivisionRing.trivial
