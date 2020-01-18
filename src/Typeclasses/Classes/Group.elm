module Typeclasses.Classes.Group exposing (Group)

{-| Group typeclass definition and its instances for basic types.


# Definition

@docs Group

-}

import Either exposing (Either(..))
import Set exposing (Set)
import Task exposing (Task)
import Typeclasses.Classes.Monoid exposing (Monoid)
import Typeclasses.Classes.Semigroup as Semigroup exposing (Semigroup)


{-| Explicit typeclass which implements monoid operations for type `a`.
-}
type alias Group a =
    { monoid : Monoid a
    , inverse : a -> a -> a
    }
