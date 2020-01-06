module Typeclasses.Classes.Bounded exposing
    ( Bounded
    , interval
    , int, char, bool, order, unit
    , tuple2, tuple3
    )

{-| Bounded typeclass definition and its instances for basic types.


# Definition

@docs Bounded


# Construction utilities

@docs interval


# Instances

@docs int, char, bool, order, unit


# Composites

@docs tuple2, tuple3

-}


{-| Explicit typeclass which implements a bounded type `a`.
-}
type alias Bounded a =
    { minBound : a
    , maxBound : a
    }



-- * Constructors
-------------------------


{-| Construct an instance from the interval `(minBound, maxBound)`.
-}
interval : ( a, a ) -> Bounded a
interval ( minBound, maxBound ) =
    Bounded minBound maxBound



-- * Instances
-------------------------


{-| Instance for `Int`.
-}
int : Bounded Int
int =
    interval ( -2147483648, 2147483647 )


{-| Instance for `Char`.
-}
char : Bounded Char
char =
    interval ( '\u{0000}', '\u{10FFFF}' )


{-| Instance for `Bool`.
-}
bool : Bounded Bool
bool =
    Bounded False True


{-| Instance for `Order`.
-}
order : Bounded Order
order =
    Bounded LT GT


{-| Instance for `()`.
-}
unit : Bounded ()
unit =
    Bounded () ()



-- * Composites
-------------------------


{-| Instance for tuple (pair), with instances for its members provided.
-}
tuple2 : Bounded a -> Bounded b -> Bounded ( a, b )
tuple2 boundedA boundedB =
    let
        minBound =
            ( boundedA.minBound, boundedB.minBound )

        maxBound =
            ( boundedA.maxBound, boundedB.maxBound )
    in
    Bounded minBound maxBound


{-| Instance for tuple (triple), with instances for its members provided.
-}
tuple3 : Bounded a -> Bounded b -> Bounded c -> Bounded ( a, b, c )
tuple3 boundedA boundedB boundedC =
    let
        minBound =
            ( boundedA.minBound, boundedB.minBound, boundedC.minBound )

        maxBound =
            ( boundedA.maxBound, boundedB.maxBound, boundedC.maxBound )
    in
    Bounded minBound maxBound
