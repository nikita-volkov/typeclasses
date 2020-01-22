module Typeclasses.Classes.AbelianGroup exposing
    ( AbelianGroup
    , numberSum
    )

{-| Group typeclass definition and its instances for basic types.


# Definition

@docs Group

-}

import Typeclasses.Classes.Group exposing (Group)


{-| Explicit typeclass which implements group operations for type `a`.
-}
type alias AbelianGroup a =
    Group a


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : AbelianGroup number
numberSum =
    Typeclasses.Classes.Group.numberSum
