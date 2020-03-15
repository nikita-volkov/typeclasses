module Typeclasses.Classes.Ring exposing
    ( Ring
    , numberRing, trivialRing, exclusiveOrRing
    , CommutativeRing(..)
    )

{-| Ring typeclass definition and its instances for basic types.


# Definition

@docs Ring

#Instances

@docs numberRing, trivialRing, exclusiveOrRing

-}

import Typeclasses.Classes.Group
import Typeclasses.Classes.Monoid


{-| Explicit typeclass which implements group operations for type `a`.
-}
type alias Ring a =
    { addition : Typeclasses.Classes.Group.AbelianGroup a
    , multiplication : Typeclasses.Classes.Monoid.Monoid a
    }


type CommutativeRing a
    = CommutativeRing
        { addition : Typeclasses.Classes.Group.AbelianGroup a
        , multiplication : Typeclasses.Classes.Monoid.CommutativeMonoid a
        }


{-| Construct real number ring
-}
numberRing : CommutativeRing number
numberRing =
    { addition = Typeclasses.Classes.Group.numberSum
    , multiplication = Typeclasses.Classes.Monoid.numberProduct
    }
        |> CommutativeRing


{-| Construct trivial ring
-}
trivialRing : CommutativeRing ()
trivialRing =
    { addition = Typeclasses.Classes.Group.trivialGroup
    , multiplication = Typeclasses.Classes.Monoid.unit
    }
        |> CommutativeRing


{-| Construct exclusive all ring
-}
exclusiveOrRing : CommutativeRing Bool
exclusiveOrRing =
    { addition = Typeclasses.Classes.Group.exclusiveOr
    , multiplication = Typeclasses.Classes.Monoid.all
    }
        |> CommutativeRing
