module CommutativeMonoid exposing
    ( CommutativeMonoid(..)
    , numberProduct, intProduct, numberSum, intSum, unit, modularArithmetic, all, any, exclusiveOr, commutativeSemigroupAndIdentity
    )

{-| Commutative Monoid typeclass definition and its instances for basic types.


# Definition

@docs CommutativeMonoid


# Instances

@docs numberProduct, intProduct, numberSum, intSum, unit, modularArithmetic, all, any, exclusiveOr, commutativeSemigroupAndIdentity

-}

import Monoid
import Semigroup


{-| Explicit typeclass which implements monoid operations for type `a` when the operation is commutative.
-}
type CommutativeMonoid a
    = CommutativeMonoid (Monoid.Monoid a)


{-| Construct an instance by specifying a commutative semigroup instance and an identity value.
-}
commutativeSemigroupAndIdentity : Semigroup.Semigroup a -> a -> CommutativeMonoid a
commutativeSemigroupAndIdentity commutativeSemigroup identity =
    { semigroup = commutativeSemigroup
    , identity = identity
    , concat = List.foldl commutativeSemigroup identity
    }
        |> CommutativeMonoid


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements multiplication.
-}
numberProduct : CommutativeMonoid number
numberProduct =
    CommutativeMonoid Monoid.numberProduct


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : CommutativeMonoid number
numberSum =
    CommutativeMonoid Monoid.numberSum


{-| Instance for integers under the multiplication operation.
-}
intProduct : CommutativeMonoid Int
intProduct =
    numberProduct


{-| Instance for integers under the sum operation.
-}
intSum : CommutativeMonoid Int
intSum =
    numberSum


{-| Instance for all
-}
all : CommutativeMonoid Bool
all =
    CommutativeMonoid Monoid.all


{-| Instance for any
-}
any : CommutativeMonoid Bool
any =
    CommutativeMonoid Monoid.any


{-| Instance for trivial monoid
-}
unit : CommutativeMonoid ()
unit =
    CommutativeMonoid Monoid.unit


{-| Instance for exclusiveOr
-}
exclusiveOr : CommutativeMonoid Bool
exclusiveOr =
    CommutativeMonoid Monoid.exclusiveOr


{-| Instance for modularArithmetic
-}
modularArithmetic : Int -> CommutativeMonoid Int
modularArithmetic divisor =
    Monoid.modularArithmetic divisor
        |> CommutativeMonoid
