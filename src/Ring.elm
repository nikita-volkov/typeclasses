module Ring exposing
    ( Ring
    , exclusiveOr, number, trivial
    )

{-| Ring typeclass definition and its instances for basic types.


# Definition

@docs Ring

#Instances

@docs exclusiveOr, number, trivial

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
number : Ring number
number =
    { addition = AbelianGroup.numberSum
    , multiplication = Monoid.numberProduct
    }


{-| Construct trivial ring
-}
trivial : Ring ()
trivial =
    { addition = AbelianGroup.trivial
    , multiplication = Monoid.unit
    }


{-| Construct exclusive all ring
-}
exclusiveOr : Ring Bool
exclusiveOr =
    { addition = AbelianGroup.exclusiveOr
    , multiplication = Monoid.all
    }
