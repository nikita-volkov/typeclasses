module Ring exposing
    ( Ring
    , exclusiveOrRing, numberRing, trivialRing
    )

{-| Ring typeclass definition and its instances for basic types.


# Definition

@docs Ring

#Instances

@docs exclusiveOrRing, numberRing, trivialRing

-}

import AbelianGroup
import CommutativeRing
import Monoid


{-| Explicit typeclass which implements ring operations for type `a`.
-}
type alias Ring a =
    { addition : AbelianGroup.AbelianGroup a
    , multiplication : Monoid.Monoid a
    }


{-| Construct real number ring
-}
numberRing : Ring number
numberRing =
    let
        (CommutativeRing.CommutativeRing ring) =
            CommutativeRing.numberRing
    in
    { addition = ring.addition
    , multiplication = Monoid.numberProduct
    }


{-| Construct trivial ring
-}
trivialRing : Ring ()
trivialRing =
    let
        (CommutativeRing.CommutativeRing ring) =
            CommutativeRing.trivialRing
    in
    { addition = ring.addition
    , multiplication = Monoid.unit
    }


{-| Construct exclusive all ring
-}
exclusiveOrRing : Ring Bool
exclusiveOrRing =
    let
        (CommutativeRing.CommutativeRing ring) =
            CommutativeRing.exclusiveOrRing
    in
    { addition = ring.addition
    , multiplication = Monoid.exclusiveOr
    }
