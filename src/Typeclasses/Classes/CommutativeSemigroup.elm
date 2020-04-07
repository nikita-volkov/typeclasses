module Typeclasses.Classes.CommutativeSemigroup exposing
    ( CommutativeSemigroup(..)
    , and
    , intProduct
    , intSum
    , modularArithmetic
    , numberProduct
    , numberSum
    , or
    , setIntersection
    , setUnion
    , unit
    , xor
    )

import Set
import Typeclasses.Classes.Semigroup


type CommutativeSemigroup a
    = CommutativeSemigroup (Typeclasses.Classes.Semigroup.Semigroup a)


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements multiplication.
-}
numberProduct : CommutativeSemigroup number
numberProduct =
    Typeclasses.Classes.Semigroup.prepend (*)
        |> CommutativeSemigroup


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : CommutativeSemigroup number
numberSum =
    Typeclasses.Classes.Semigroup.prepend (+)
        |> CommutativeSemigroup


{-| Instance for integers under the multiplication operation.
-}
intProduct : CommutativeSemigroup Int
intProduct =
    numberProduct


{-| Instance for integers under the sum operation.
-}
intSum : CommutativeSemigroup Int
intSum =
    numberSum


{-| Instance for set under the union operation.
-}
setUnion : CommutativeSemigroup (Set.Set comparable)
setUnion =
    Typeclasses.Classes.Semigroup.prepend Set.union
        |> CommutativeSemigroup


{-| Instance for set under the intersection operation.
-}
setIntersection : CommutativeSemigroup (Set.Set comparable)
setIntersection =
    Typeclasses.Classes.Semigroup.prepend Set.intersect
        |> CommutativeSemigroup


{-| Instance for and
-}
and : CommutativeSemigroup Bool
and =
    Typeclasses.Classes.Semigroup.prepend (&&)
        |> CommutativeSemigroup


{-| Instance for or
-}
or : CommutativeSemigroup Bool
or =
    Typeclasses.Classes.Semigroup.prepend (||)
        |> CommutativeSemigroup


{-| Instance for trivial semigroup
-}
unit : CommutativeSemigroup ()
unit =
    Typeclasses.Classes.Semigroup.prepend (\() () -> ())
        |> CommutativeSemigroup


xor : CommutativeSemigroup Bool
xor =
    Typeclasses.Classes.Semigroup.prepend Basics.xor
        |> CommutativeSemigroup


{-| Instance for modularArithmetic semigroup
-}
modularArithmetic : Int -> CommutativeSemigroup Int
modularArithmetic divisor =
    Typeclasses.Classes.Semigroup.prepend
        (\dividendOne dividendTwo ->
            dividendOne
                + dividendTwo
                |> Basics.modBy divisor
        )
        |> CommutativeSemigroup
