module Monoid exposing
    ( Monoid
    , identityAndConcat, semigroupAndIdentity, appendable
    , map
    , string, maybeFirst, list, cmd, sub, task, composition, setDifference, setUnion, all, any, exclusiveOr, intProduct, intSum, modularArithmetic, numberProduct, numberSum, unit
    )

{-| Monoid typeclass definition and its instances for basic types.


# Definition

@docs Monoid


# Construction utilities

@docs identityAndConcat, semigroupAndIdentity, appendable


# Instance transformation utilities

@docs map


# Instances

@docs string, maybeFirst, list, cmd, sub, task, composition, setDifference, setUnion, all, any, exclusiveOr, intProduct, intSum, modularArithmetic, numberProduct, numberSum, unit

-}

import Semigroup
import Set
import Task


{-| Explicit typeclass which implements monoid operations for type `a`.
-}
type alias Monoid a =
    { semigroup : Semigroup.Semigroup a
    , identity : a
    , concat : List a -> a
    }



-- * Construction utilities
-------------------------


{-| Construct an instance by specifying identity value and a concatenation operation.
-}
identityAndConcat : a -> (List a -> a) -> Monoid a
identityAndConcat identity concat =
    { semigroup = Semigroup.concat concat
    , identity = identity
    , concat = concat
    }


{-| Construct an instance by specifying a semigroup instance and an identity value.
-}
semigroupAndIdentity : Semigroup.Semigroup a -> a -> Monoid a
semigroupAndIdentity semigroup identity =
    { semigroup = semigroup
    , identity = identity
    , concat = List.foldl semigroup identity
    }


{-| Construct an instance for any type which satisfies Elm's `appendable` magic constraint,
by providing an identity value.
-}
appendable : appendable -> Monoid appendable
appendable =
    semigroupAndIdentity Semigroup.appendable



-- * Transformation utilities
-------------------------


{-| Map over the owner type of an instance to produce a new instance.

You need to provide both a covariant and a contravariant mapping
(i.e., `(a -> b)` and `(b -> a)`).

-}
map : (a -> b) -> (b -> a) -> Monoid a -> Monoid b
map aToB bToA monoidOfA =
    semigroupAndIdentity
        (Semigroup.map aToB bToA monoidOfA.semigroup)
        (aToB monoidOfA.identity)



-- * Instances
-------------------------


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements multiplication.
-}
numberProduct : Monoid number
numberProduct =
    { semigroup = Semigroup.numberProduct, identity = 1, concat = List.product }


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : Monoid number
numberSum =
    { semigroup = Semigroup.numberSum, identity = 0, concat = List.sum }


{-| Instance for integers under the multiplication operation.
-}
intProduct : Monoid Int
intProduct =
    numberProduct


{-| Instance for integers under the sum operation.
-}
intSum : Monoid Int
intSum =
    numberSum


{-| Instance for all
-}
all : Monoid Bool
all =
    semigroupAndIdentity Semigroup.and True


{-| Instance for any
-}
any : Monoid Bool
any =
    semigroupAndIdentity Semigroup.or False


{-| Instance for trivial monoid
-}
unit : Monoid ()
unit =
    semigroupAndIdentity Semigroup.unit ()


{-| Instance for exclusiveOr
-}
exclusiveOr : Monoid Bool
exclusiveOr =
    semigroupAndIdentity Semigroup.xor False


{-| Instance for modularArithmetic
-}
modularArithmetic : Int -> Monoid Int
modularArithmetic divisor =
    semigroupAndIdentity (Semigroup.modularArithmetic divisor) 0


{-| Instance for strings under the appending operation.
-}
string : Monoid String
string =
    appendable ""


{-| Instance for maybe, which chooses the first `Just` value.
-}
maybeFirst : Monoid (Maybe a)
maybeFirst =
    semigroupAndIdentity Semigroup.maybeFirst Nothing


{-| Instance for list under concatenation.
-}
list : Monoid (List a)
list =
    appendable []


{-| Instance for set under the union operation.
-}
setUnion : Monoid (Set.Set comparable)
setUnion =
    semigroupAndIdentity Semigroup.setUnion Set.empty


{-| Instance for set under the difference operation.
-}
setDifference : Monoid (Set.Set comparable)
setDifference =
    semigroupAndIdentity Semigroup.setDifference Set.empty


{-| Instance for commands under the batch operation.
-}
cmd : Monoid (Cmd msg)
cmd =
    identityAndConcat Cmd.none Cmd.batch


{-| Instance for subscriptions under the batch operation.
-}
sub : Monoid (Sub msg)
sub =
    identityAndConcat Sub.none Sub.batch


{-| Instance for tasks, which sequentially executes them and groups the results.
-}
task : Monoid a -> Monoid (Task.Task x a)
task monoidOfA =
    { semigroup = Semigroup.task monoidOfA.semigroup
    , identity = Task.succeed monoidOfA.identity
    , concat = Task.sequence >> Task.map monoidOfA.concat
    }


{-| Instance for a -> a function
-}
composition : Monoid (a -> a)
composition =
    semigroupAndIdentity Semigroup.composition Basics.identity
