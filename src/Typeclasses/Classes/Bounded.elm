module Typeclasses.Classes.Bounded exposing
    ( Bounded
    , bounds
    , int, char
    , tuple2, tuple3
    , bool
    )

{-| Bounded typeclass definition and its instances for basic types.


# Definition

@docs Bounded


# Construction utilities

@docs bounds


# Instances

@docs int, char


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
    bounds ( 0, 0 )


{-| Instance for `Float`.
-}
char : Bounded Char
char =
    bounds ( 'a', 'a' )


{-| Instance for `Bool`.
-}
bool : Bounded Bool
bool =
    bounds ( False, True )



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
