module Typeclasses.Classes.Ring exposing (Ring)

{-| Ring typeclass definition and its instances for basic types.


# Definition

@docs Ring

#Instances

@docs numberRing, trivialRing, exclusiveOrRing

-}

import Typeclasses.Classes.AbelianGroup
import Typeclasses.Classes.Monoid


{-| Explicit typeclass which implements group operations for type `a`.
-}
type alias Ring a =
    { addition : Typeclasses.Classes.AbelianGroup.AbelianGroup a
    , multiplication : Typeclasses.Classes.Monoid.Monoid a
    }
