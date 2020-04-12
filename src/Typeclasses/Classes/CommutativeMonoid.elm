module Typeclasses.Classes.CommutativeMonoid exposing
    ( CommutativeMonoid(..)
    , all
    , any
    , exclusiveOr
    , intProduct
    , intSum
    , modularArithmetic
    , numberProduct
    , numberSum
    , unit
    )

import Either exposing (Either(..))
import Typeclasses.Classes.CommutativeSemigroup


{-| Commutative Monoid typeclass definition and its instances for basic types.


# Definition

@docs CommutativeMonoid


# Construction utilities

@docs commutativeSemigroupAndIdentity


# Instance transformation utilities

@docs map


# Instances

@docs numberProduct, numberSum, intProduct, intSum, all, any, unit, exclusiveOr, modularArithmetic

-}
type CommutativeMonoid a
    = CommutativeMonoid
        { semigroup : Typeclasses.Classes.CommutativeSemigroup.CommutativeSemigroup a
        , identity : a
        , concat : List a -> a
        }


{-| Construct an instance by specifying a commutative semigroup instance and an identity value.
-}
commutativeSemigroupAndIdentity : Typeclasses.Classes.CommutativeSemigroup.CommutativeSemigroup a -> a -> CommutativeMonoid a
commutativeSemigroupAndIdentity commutativeSemigroup identity =
    let
        (Typeclasses.Classes.CommutativeSemigroup.CommutativeSemigroup semigroup) =
            commutativeSemigroup
    in
    { semigroup = commutativeSemigroup
    , identity = identity
    , concat = List.foldl semigroup identity
    }
        |> CommutativeMonoid


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements multiplication.
-}
numberProduct : CommutativeMonoid number
numberProduct =
    { semigroup = Typeclasses.Classes.CommutativeSemigroup.numberProduct, identity = 1, concat = List.product }
        |> CommutativeMonoid


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : CommutativeMonoid number
numberSum =
    { semigroup = Typeclasses.Classes.CommutativeSemigroup.numberSum, identity = 0, concat = List.sum }
        |> CommutativeMonoid


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
    commutativeSemigroupAndIdentity Typeclasses.Classes.CommutativeSemigroup.and True


{-| Instance for any
-}
any : CommutativeMonoid Bool
any =
    commutativeSemigroupAndIdentity Typeclasses.Classes.CommutativeSemigroup.or False


{-| Instance for trivial monoid
-}
unit : CommutativeMonoid ()
unit =
    commutativeSemigroupAndIdentity Typeclasses.Classes.CommutativeSemigroup.unit ()


{-| Instance for exclusiveOr
-}
exclusiveOr : CommutativeMonoid Bool
exclusiveOr =
    commutativeSemigroupAndIdentity Typeclasses.Classes.CommutativeSemigroup.xor False


{-| Instance for modularArithmetic
-}
modularArithmetic : Int -> CommutativeMonoid Int
modularArithmetic divisor =
    commutativeSemigroupAndIdentity (Typeclasses.Classes.CommutativeSemigroup.modularArithmetic divisor) 0
