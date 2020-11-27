module Field exposing
    ( Field(..)
    , exclusiveOrField, numberField, trivialField
    )

{-| Field typeclass definition and its instances for basic types.


# Definition

@docs Field

-}

import CommutativeRing


{-| Explicit typeclass which implements group operations for type `a`.
-}
type Field a
    = Field (CommutativeRing.CommutativeRing a)


{-| Construct real number field
-}
numberField : Field number
numberField =
    Field CommutativeRing.numberRing


{-| Construct trivial field
-}
trivialField : Field ()
trivialField =
    Field CommutativeRing.trivialRing


{-| Construct exclusive all field
-}
exclusiveOrField : Field Bool
exclusiveOrField =
    Field CommutativeRing.exclusiveOrRing
