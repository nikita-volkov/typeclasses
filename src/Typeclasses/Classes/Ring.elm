module Typeclasses.Classes.Ring exposing
    ( Ring
    , exclusiveOrRing, numberRing, trivialRing
    )

{-| Ring typeclass definition and its instances for basic types.


# Definition

@docs Ring

-}

import Typeclasses.Classes.AbelianGroup
import Typeclasses.Classes.CommutativeRing
import Typeclasses.Classes.Monoid


{-| Explicit typeclass which implements group operations for type `a`.
-}
type alias Ring a =
    { addition : Typeclasses.Classes.AbelianGroup.AbelianGroup a
    , multiplication : Typeclasses.Classes.Monoid.Monoid a
    }


{-| Construct real number ring
-}
numberRing : Ring number
numberRing =
    let
        (Typeclasses.Classes.CommutativeRing.CommutativeRing ring) =
            Typeclasses.Classes.CommutativeRing.numberRing
    in
    { addition = ring.addition
    , multiplication = Typeclasses.Classes.Monoid.numberProduct
    }


{-| Construct trivial ring
-}
trivialRing : Ring ()
trivialRing =
    let
        (Typeclasses.Classes.CommutativeRing.CommutativeRing ring) =
            Typeclasses.Classes.CommutativeRing.trivialRing
    in
    { addition = ring.addition
    , multiplication = Typeclasses.Classes.Monoid.unit
    }


{-| Construct exclusive all ring
-}
exclusiveOrRing : Ring Bool
exclusiveOrRing =
    let
        (Typeclasses.Classes.CommutativeRing.CommutativeRing ring) =
            Typeclasses.Classes.CommutativeRing.exclusiveOrRing
    in
    { addition = ring.addition
    , multiplication = Typeclasses.Classes.Monoid.exclusiveOr
    }
