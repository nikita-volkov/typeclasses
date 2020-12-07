module Field exposing
    ( Field(..)
    , numberField, trivialField
    )

{-| Field typeclass definition and its instances for basic types.


# Definition

@docs Field

#Instances

@docs numberField, trivialField

-}

import CommutativeDivisionRing


{-| Explicit typeclass which implements group operations for type `a`.
-}
type Field a
    = Field (CommutativeDivisionRing.CommutativeDivisionRing a)


{-| Construct real number field
-}
numberField : Field Float
numberField =
    Field CommutativeDivisionRing.floatCommutativeDivisionRing


{-| Construct trivial field
-}
trivialField : Field ()
trivialField =
    Field CommutativeDivisionRing.trivialCommutativeDivisionRing
