module Typeclasses.Classes.Field exposing
    ( Field(..)
    , exclusiveOrField, numberField, trivialField
    )

{-| Field typeclass definition and its instances for basic types.


# Definition

@docs Field

-}

import Typeclasses.Classes.CommutativeRing


{-| Explicit typeclass which implements group operations for type `a`.
-}
type Field a
    = Field (Typeclasses.Classes.CommutativeRing.CommutativeRing a)


{-| Construct real number field
-}
numberField : Field number
numberField =
    Field Typeclasses.Classes.CommutativeRing.numberRing


{-| Construct trivial field
-}
trivialField : Field ()
trivialField =
    Field Typeclasses.Classes.CommutativeRing.trivialRing


{-| Construct exclusive all field
-}
exclusiveOrField : Field Bool
exclusiveOrField =
    Field Typeclasses.Classes.CommutativeRing.exclusiveOrRing
