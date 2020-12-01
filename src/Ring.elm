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
    { addition = AbelianGroup.numberSum
    , multiplication = Monoid.numberProduct
    }


{-| Construct trivial ring
-}
trivialRing : Ring ()
trivialRing =
    { addition = AbelianGroup.trivialGroup
    , multiplication = Monoid.unit
    }


{-| Construct exclusive all ring
-}
exclusiveOrRing : Ring Bool
exclusiveOrRing =
    { addition = AbelianGroup.exclusiveOr
    , multiplication = Monoid.all
    }
