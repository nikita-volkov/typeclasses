module Typeclasses.Classes.CommutativeMonoid exposing
    ( CommutativeMonoid
    , identityAndConcat, numberProduct, numberSum
    , map
    , intProduct, intSum, string, maybeFirst, list, setUnion, setDifference, cmd, sub, task
    )

{-| CommutativeMonoid typeclass definition and its instances for basic types.


# Definition

@docs CommutativeMonoid


# Construction utilities

@docs identityAndConcat, semigroupAndIdentity, appendable, numberProduct, numberSum


# Instance transformation utilities

@docs map


# Instances

@docs intProduct, intSum, string, maybeFirst, list, setUnion, setDifference, cmd, sub, task

-}

import Either exposing (Either(..))
import Set exposing (Set)
import Task exposing (Task)
import Typeclasses.Classes.Monoid


{-| Explicit typeclass which implements monoid operations for type `a`.
-}
type alias CommutativeMonoid a =
    Typeclasses.Classes.Monoid.Monoid a



-- * Construction utilities
-------------------------


{-| Construct an instance by specifying identity value and a concatenation operation.
-}
identityAndConcat : a -> (List a -> a) -> Typeclasses.Classes.Monoid.Monoid a
identityAndConcat =
    Typeclasses.Classes.Monoid.identityAndConcat


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements multiplication.
-}
numberProduct : Typeclasses.Classes.Monoid.Monoid number
numberProduct =
    Typeclasses.Classes.Monoid.numberProduct


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : Typeclasses.Classes.Monoid.Monoid number
numberSum =
    Typeclasses.Classes.Monoid.numberSum



-- * Transformation utilities
-------------------------


{-| Map over the owner type of an instance to produce a new instance.

You need to provide both a covariant and a contravariant mapping
(i.e., `(a -> b)` and `(b -> a)`).

-}
map : (a -> b) -> (b -> a) -> Typeclasses.Classes.Monoid.Monoid a -> Typeclasses.Classes.Monoid.Monoid b
map =
    Typeclasses.Classes.Monoid.map



-- * Instances
-------------------------


{-| Instance for integers under the multiplication operation.
-}
intProduct : Typeclasses.Classes.Monoid.Monoid Int
intProduct =
    numberProduct


{-| Instance for integers under the sum operation.
-}
intSum : Typeclasses.Classes.Monoid.Monoid Int
intSum =
    numberSum


{-| Instance for set under the union operation.
-}
setUnion : Typeclasses.Classes.Monoid.Monoid (Set comparable)
setUnion =
    Typeclasses.Classes.Monoid.setUnion


{-| Instance for set under the difference operation.
-}
setDifference : Typeclasses.Classes.Monoid.Monoid (Set comparable)
setDifference =
    Typeclasses.Classes.Monoid.setDifference


{-| Instance for commands under the batch operation.
-}
cmd : Typeclasses.Classes.Monoid.Monoid (Cmd msg)
cmd =
    identityAndConcat Cmd.none Cmd.batch


{-| Instance for subscriptions under the batch operation.
-}
sub : Typeclasses.Classes.Monoid.Monoid (Sub msg)
sub =
    identityAndConcat Sub.none Sub.batch


{-| Instance for tasks, which sequentially executes them and groups the results.
-}
task : Typeclasses.Classes.Monoid.Monoid a -> Typeclasses.Classes.Monoid.Monoid (Task x a)
task =
    Typeclasses.Classes.Monoid.task
