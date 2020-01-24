module Typeclasses.Classes.Ring exposing
    ( Ring
    , numberRing
    )

{-| Ring typeclass definition and its instances for basic types.


# Definition

@docs Ring

-}

import Typeclasses.Classes.AbelianGroup
import Typeclasses.Classes.Monoid


{-| Explicit typeclass which implements group operations for type `a`.
-}
type alias Ring a =
    { addition : Typeclasses.Classes.AbelianGroup.AbelianGroup a
    , multiplication : Typeclasses.Classes.Monoid.Monoid a
    }


numberRing : Ring number
numberRing =
    { addition = Typeclasses.Classes.AbelianGroup.numberSum
    , multiplication = Typeclasses.Classes.Monoid.numberProduct
    }
