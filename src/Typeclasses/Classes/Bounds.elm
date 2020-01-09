module Typeclasses.Classes.Bounds exposing
    ( Bounds
    , int, char, bool, order, unit
    , tuple2, tuple3
    , map
    )

{-| Bounds typeclass definition and its instances for basic types.


# Definition

@docs Bounds


# Instances

@docs int, char, bool, order, unit


# Composites

@docs tuple2, tuple3


# Instance transformation utilities

@docs map

-}


{-| Explicit typeclass which implements a bounded type `a`.
-}
type alias Bounds a =
    { min : a
    , max : a
    }



-- * Instances
-------------------------


{-| Instance for `Int`.
-}
int : Bounds Int
int =
    Bounds -2147483648 2147483647


{-| Instance for `Char`.
-}
char : Bounds Char
char =
    Bounds '\u{0000}' '\u{10FFFF}'


{-| Instance for `Bool`.
-}
bool : Bounds Bool
bool =
    Bounds False True


{-| Instance for `Order`.
-}
order : Bounds Order
order =
    Bounds LT GT


{-| Instance for `()`.
-}
unit : Bounds ()
unit =
    Bounds () ()



-- * Composites
-------------------------


{-| Instance for tuple (pair), with instances for its members provided.
-}
tuple2 : Bounds a -> Bounds b -> Bounds ( a, b )
tuple2 boundedA boundedB =
    let
        minBound =
            ( boundedA.min, boundedB.min )

        maxBound =
            ( boundedA.max, boundedB.max )
    in
    Bounds minBound maxBound


{-| Instance for tuple (triple), with instances for its members provided.
-}
tuple3 : Bounds a -> Bounds b -> Bounds c -> Bounds ( a, b, c )
tuple3 boundedA boundedB boundedC =
    let
        minBound =
            ( boundedA.min, boundedB.min, boundedC.min )

        maxBound =
            ( boundedA.max, boundedB.max, boundedC.max )
    in
    Bounds minBound maxBound



-- * Transformations
-------------------------


{-| Map over the owner type of an instance to produce a new instance.

For example, to created a bounded record type:

    map (\v -> { x = v }) int == Bounds { x = int.min } { x = int.max }

-}
map : (a -> b) -> Bounds a -> Bounds b
map transform boundsA =
    Bounds (transform boundsA.min) (transform boundsA.max)
