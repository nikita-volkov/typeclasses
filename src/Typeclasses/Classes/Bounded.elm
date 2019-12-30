module Typeclasses.Classes.Bounded exposing
    ( Bounded
    , bounds
    , int, char, bool, order, unit
    , tuple2, tuple3
    )

{-| Bounded typeclass definition and its instances for basic types.


# Definition

@docs Bounded


# Construction utilities

@docs bounds


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


{-| Construct an instance from the upper and lower bounds.
-}
bounds : ( a, a ) -> Bounded a
bounds ( minBound, maxBound ) =
    Bounded minBound maxBound



-- * Instances
-------------------------


{-| Instance for `Int`.
-}
int : Bounded Int
int =
    bounds ( -2147483648, 2147483647 )


{-| Instance for `Float`.
-}
char : Bounded Char
char =
    bounds ( '\u{0000}', '\u{10FFFF}' )


{-| Instance for `Bool`.
-}
bool : Bounded Bool
bool =
    bounds ( False, True )


{-| Instance for `Order`.
-}
order : Bounded Order
order =
    bounds ( LT, GT )


{-| Instance for `()`.
-}
unit : Bounded ()
unit =
    bounds ( (), () )



-- * Composites
-------------------------


{-| Instance for tuple, with instances for its members provided.
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


{-| Instance for tuple, with instances for its members provided.
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
