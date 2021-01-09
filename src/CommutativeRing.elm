module CommutativeRing exposing
    ( CommutativeRing(..)
    , number, trivial, exclusiveOr
    )

{-| Commutative Ring typeclass definition and its instances for basic types.


# Definition

@docs CommutativeRing

#Instances

@docs number, trivial, exclusiveOr

-}

import Ring


{-| Explicit typeclass which implements ring operations for type `a` when the multiplication operation is commutative.
-}
type CommutativeRing a
    = CommutativeRing (Ring.Ring a)


{-| Construct real number ring
-}
number : CommutativeRing number
number =
    CommutativeRing Ring.number


{-| Construct trivial ring
-}
trivial : CommutativeRing ()
trivial =
    CommutativeRing Ring.trivial


{-| Construct exclusive all ring
-}
exclusiveOr : CommutativeRing Bool
exclusiveOr =
    CommutativeRing Ring.exclusiveOr
