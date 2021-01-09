module Semigroup exposing
    ( Semigroup
    , concat, appendable
    , map
    , string, maybeFirst, list, cmd, sub, task, composition, setDifference, and, intProduct, intSum, modularArithmetic, numberProduct, numberSum, or, setIntersection, setUnion, unit, xor
    )

{-| Semigroup typeclass definition and its instances for basic types.


# Definition

@docs Semigroup


# Construction utilities

@docs concat, appendable


# Instance transformation utilities

@docs map


# Instances

@docs string, maybeFirst, list, cmd, sub, task, composition, setDifference, and, intProduct, intSum, modularArithmetic, numberProduct, numberSum, or, setIntersection, setUnion, unit, xor

-}

import Set
import Task


{-| Explicit typeclass which implements semigroup operations for type `a`.
-}
type alias Semigroup a =
    a -> a -> a



-- * Constructors
-------------------------


{-| Construct from a concatenation function.
-}
concat : (List a -> a) -> Semigroup a
concat concat_ =
    \l r -> concat_ [ l, r ]


{-| Construct an instance for any type which satisfies Elm's `appendable` magic constraint.
-}
appendable : Semigroup appendable
appendable =
    (++)



-- * Transformation utilities
-------------------------


{-| Map over the owner type of an instance to produce a new instance.

You need to provide both a covariant and a contravariant mapping
(i.e., `(a -> b)` and `(b -> a)`).

-}
map : (a -> b) -> (b -> a) -> Semigroup a -> Semigroup b
map aToB bToA semigroupOfA =
    \lb rb -> aToB (semigroupOfA (bToA lb) (bToA rb))



-- * Instances
-------------------------


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements multiplication.
-}
numberProduct : Semigroup number
numberProduct =
    (*)


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : Semigroup number
numberSum =
    (+)


{-| Instance for integers under the multiplication operation.
-}
intProduct : Semigroup Int
intProduct =
    numberProduct


{-| Instance for integers under the sum operation.
-}
intSum : Semigroup Int
intSum =
    numberSum


{-| Instance for set under the union operation.
-}
setUnion : Semigroup (Set.Set comparable)
setUnion =
    Set.union


{-| Instance for set under the intersection operation.
-}
setIntersection : Semigroup (Set.Set comparable)
setIntersection =
    Set.intersect


{-| Instance for and
-}
and : Semigroup Bool
and =
    (&&)


{-| Instance for or
-}
or : Semigroup Bool
or =
    (||)


{-| Instance for trivial semigroup
-}
unit : Semigroup ()
unit =
    \() () -> ()


{-| Instance for xor
-}
xor : Semigroup Bool
xor =
    Basics.xor


{-| Instance for modularArithmetic semigroup
-}
modularArithmetic : Int -> Semigroup Int
modularArithmetic divisor =
    \dividendOne dividendTwo ->
        dividendOne
            + dividendTwo
            |> Basics.modBy divisor


{-| Instance for strings under the appending operation.
-}
string : Semigroup String
string =
    appendable


{-| Instance for maybe, which chooses the first `Just` value.
-}
maybeFirst : Semigroup (Maybe a)
maybeFirst =
    \l r ->
        case l of
            Nothing ->
                r

            _ ->
                l


{-| Instance for list under concatenation.
-}
list : Semigroup (List a)
list =
    appendable


{-| Instance for set under the difference operation.
-}
setDifference : Semigroup (Set.Set comparable)
setDifference =
    Set.diff


{-| Instance for commands under the batch operation.
-}
cmd : Semigroup (Cmd msg)
cmd =
    concat Cmd.batch


{-| Instance for subscriptions under the batch operation.
-}
sub : Semigroup (Sub msg)
sub =
    concat Sub.batch


{-| Instance for tasks, which sequentially executes them and groups the results.
-}
task : Semigroup a -> Semigroup (Task.Task x a)
task semigroupOfA =
    \l r -> l |> Task.andThen (\la -> Task.map (semigroupOfA la) r)


{-| Instance for a -> a function
-}
composition : Semigroup (a -> a)
composition =
    (>>)
