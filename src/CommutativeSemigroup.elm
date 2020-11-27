module CommutativeSemigroup exposing
    ( CommutativeSemigroup(..)
    , numberProduct, intProduct, numberSum, intSum, setUnion, setIntersection, and, or, unit, xor, modularArithmetic
    )

{-| Commutative Semigroup typeclass definition and its instances for basic types.


# Definition

@docs CommutativeSemigroup


# Instances

@docs numberProduct, intProduct, numberSum, intSum, setUnion, setIntersection, and, or, unit, xor, modularArithmetic

-}

import Set


{-| Explicit typeclass which implements semigroup operations for type `a` when the operation is commutative.
-}
type CommutativeSemigroup a
    = CommutativeSemigroup (a -> a -> a)


{-| Construct from a prepend function.
-}
prepend : (a -> a -> a) -> CommutativeSemigroup a
prepend prepend_ =
    CommutativeSemigroup prepend_


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements multiplication.
-}
numberProduct : CommutativeSemigroup number
numberProduct =
    prepend (*)


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : CommutativeSemigroup number
numberSum =
    prepend (+)


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
    prepend Set.union


{-| Instance for set under the intersection operation.
-}
setIntersection : CommutativeSemigroup (Set.Set comparable)
setIntersection =
    prepend Set.intersect


{-| Instance for and
-}
and : CommutativeSemigroup Bool
and =
    prepend (&&)


{-| Instance for or
-}
or : CommutativeSemigroup Bool
or =
    prepend (||)


{-| Instance for trivial semigroup
-}
unit : CommutativeSemigroup ()
unit =
    prepend (\() () -> ())


{-| Instance for xor
-}
xor : CommutativeSemigroup Bool
xor =
    prepend Basics.xor


{-| Instance for modularArithmetic semigroup
-}
modularArithmetic : Int -> CommutativeSemigroup Int
modularArithmetic divisor =
    prepend
        (\dividendOne dividendTwo ->
            dividendOne
                + dividendTwo
                |> Basics.modBy divisor
        )
