module CommutativeRing exposing
    ( CommutativeRing(..)
    , numberRing, trivialRing, exclusiveOrRing
    )

{-| Commutative Ring typeclass definition and its instances for basic types.


# Definition

@docs CommutativeRing

#Instances

@docs numberRing, trivialRing, exclusiveOrRing

-}

import AbelianGroup
import CommutativeMonoid


{-| Explicit typeclass which implements ring operations for type `a` when the multiplication operation is commutative.
-}
type CommutativeRing a
    = CommutativeRing
        { addition : AbelianGroup.AbelianGroup a
        , multiplication : CommutativeMonoid.CommutativeMonoid a
        }


{-| Construct real number ring
-}
numberRing : CommutativeRing number
numberRing =
    { addition = AbelianGroup.numberSum
    , multiplication = CommutativeMonoid.numberProduct
    }
        |> CommutativeRing


{-| Construct trivial ring
-}
trivialRing : CommutativeRing ()
trivialRing =
    { addition = AbelianGroup.trivialGroup
    , multiplication = CommutativeMonoid.unit
    }
        |> CommutativeRing


{-| Construct exclusive all ring
-}
exclusiveOrRing : CommutativeRing Bool
exclusiveOrRing =
    { addition = AbelianGroup.exclusiveOr
    , multiplication = CommutativeMonoid.all
    }
        |> CommutativeRing
