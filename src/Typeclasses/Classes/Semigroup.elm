module Typeclasses.Classes.Semigroup exposing
    ( Semigroup
    , prepend, concat, appendable, numberProduct, numberSum
    , map
    , intProduct, intSum, string, maybeFirst, list, setUnion, setIntersection, setDifference, cmd, sub, task, and, or, composition, unit, xor, modularArithmetic
    )

{-| Semigroup typeclass definition and its instances for basic types.


# Definition

@docs Semigroup


# Construction utilities

@docs prepend, concat, appendable, numberProduct, numberSum


# Instance transformation utilities

@docs map


# Instances

@docs intProduct, intSum, string, maybeFirst, list, setUnion, setIntersection, setDifference, cmd, sub, task, and, or, composition, unit, xor, modularArithmetic

-}

import Either exposing (Either(..))
import Set exposing (Set)
import Task exposing (Task)


{-| Explicit typeclass which implements semigroup operations for type `a`.

Notice that the binary operation function is named "prepend" instead of "append",
because it follows the convention of having the context value come as the last value.

-}
type alias Semigroup a =
    { prepend : a -> a -> a
    }



-- * Constructors
-------------------------


{-| Construct from a prepend function.
-}
prepend : (a -> a -> a) -> Semigroup a
prepend prepend_ =
    { prepend = prepend_ }


{-| Construct from a concatenation function.
-}
concat : (List a -> a) -> Semigroup a
concat concat_ =
    prepend (\l r -> concat_ [ l, r ])


{-| Construct an instance for any type which satisfies Elm's `appendable` magic constraint.
-}
appendable : Semigroup appendable
appendable =
    prepend (++)


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements multiplication.
-}
numberProduct : Semigroup number
numberProduct =
    prepend (*)


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : Semigroup number
numberSum =
    prepend (+)



-- * Transformation utilities
-------------------------


{-| Map over the owner type of an instance to produce a new instance.

You need to provide both a covariant and a contravariant mapping
(i.e., `(a -> b)` and `(b -> a)`).

-}
map : (a -> b) -> (b -> a) -> Semigroup a -> Semigroup b
map aToB bToA semigroupOfA =
    prepend (\lb rb -> aToB (semigroupOfA.prepend (bToA lb) (bToA rb)))



-- * Instances
-------------------------


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


{-| Instance for strings under the appending operation.
-}
string : Semigroup String
string =
    appendable


{-| Instance for maybe, which chooses the first `Just` value.
-}
maybeFirst : Semigroup (Maybe a)
maybeFirst =
    prepend <|
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


{-| Instance for set under the union operation.
-}
setUnion : Semigroup (Set comparable)
setUnion =
    prepend Set.union


{-| Instance for set under the intersection operation.
-}
setIntersection : Semigroup (Set comparable)
setIntersection =
    prepend Set.intersect


{-| Instance for set under the difference operation.
-}
setDifference : Semigroup (Set comparable)
setDifference =
    prepend Set.diff


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
task : Semigroup a -> Semigroup (Task x a)
task semigroupOfA =
    prepend <| \l r -> l |> Task.andThen (\la -> Task.map (semigroupOfA.prepend la) r)


{-| Instance for and
-}
and : Semigroup Bool
and =
    prepend (&&)


{-| Instance for or
-}
or : Semigroup Bool
or =
    prepend (||)


{-| Instance for a -> a function
-}
composition : Semigroup (a -> a)
composition =
    prepend (>>)


{-| Instance for trivial semigroup
-}
unit : Semigroup ()
unit =
    prepend (\() () -> ())


xor : Semigroup Bool
xor =
    prepend Basics.xor


{-| Instance for modularArithmetic semigroup
-}
modularArithmetic : Int -> Semigroup Int
modularArithmetic divisor =
    prepend
        (\dividendOne dividendTwo ->
            dividendOne
                + dividendTwo
                |> Basics.modBy divisor
        )
