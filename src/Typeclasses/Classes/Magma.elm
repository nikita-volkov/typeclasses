module Typeclasses.Classes.Magma exposing (Magma)

{-| Semigroup typeclass definition and its instances for basic types.


# Definition

@docs Semigroup


# Construction utilities

@docs prepend, concat, appendable


# Instance transformation utilities

@docs map


# Instances

@docs string, maybeFirst, list, cmd, sub, task, composition, setDifference

-}


{-| Explicit typeclass which implements semigroup operations for type `a`.

Notice that the binary operation function is named "prepend" instead of "append",
because it follows the convention of having the context value come as the last value.

-}
type alias Magma a =
    { prepend : a -> a -> a
    }
