module Typeclasses.Classes.Bounds exposing
    ( Bounds
    , interval
    , int, char, bool, order, unit
    , tuple2, tuple3
    )

{-| Bounds typeclass definition and its instances for basic types.


# Definition

@docs Bounds


# Construction utilities

@docs interval


# Instances

@docs int, char, bool, order, unit


# Composites

@docs tuple2, tuple3

-}


{-| Explicit typeclass which implements a bounded type `a`.
-}
type alias Bounds a =
    { min : a
    , max : a
    }



-- * Constructors
-------------------------


{-| Construct an instance from the interval `(minBound, maxBound)`.
-}
interval : ( a, a ) -> Bounds a
interval ( minBound, maxBound ) =
    Bounds minBound maxBound



-- * Instances
-------------------------


{-| Instance for `Int`.
-}
int : Bounds Int
int =
    interval ( -2147483648, 2147483647 )


{-| Instance for `Char`.
-}
char : Bounds Char
char =
    interval ( '\u{0000}', '\u{10FFFF}' )


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
