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

import Ring


{-| Explicit typeclass which implements ring operations for type `a` when the multiplication operation is commutative.
-}
type CommutativeRing a
    = CommutativeRing (Ring.Ring a)


{-| Construct real number ring
-}
numberRing : CommutativeRing number
numberRing =
    CommutativeRing Ring.numberRing


{-| Construct trivial ring
-}
trivialRing : CommutativeRing ()
trivialRing =
    CommutativeRing Ring.trivialRing


{-| Construct exclusive all ring
-}
exclusiveOrRing : CommutativeRing Bool
exclusiveOrRing =
    CommutativeRing Ring.exclusiveOrRing
