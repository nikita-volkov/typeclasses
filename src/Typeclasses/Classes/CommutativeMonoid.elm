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
import Set exposing (Set)
import Task exposing (Task)
import Typeclasses.Classes.Semigroup as Semigroup exposing (Semigroup)


type CommutativeMonoid a
    = CommutativeMonoid
        { semigroup : Semigroup.CommutativeSemigroup a
        , identity : a
        , concat : List a -> a
        }


{-| Construct an instance by specifying a commutative semigroup instance and an identity value.
-}
commutativeSemigroupAndIdentity : Semigroup.CommutativeSemigroup a -> a -> CommutativeMonoid a
commutativeSemigroupAndIdentity commutativeSemigroup identity =
    let
        (Semigroup.CommutativeSemigroup semigroup) =
            commutativeSemigroup
    in
    { semigroup = commutativeSemigroup
    , identity = identity
    , concat = List.foldl semigroup.prepend identity
    }
        |> CommutativeMonoid


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements multiplication.
-}
numberProduct : CommutativeMonoid number
numberProduct =
    { semigroup = Semigroup.numberProduct, identity = 1, concat = List.product }
        |> CommutativeMonoid


{-| Construct an instance for any type which satisfies Elm's `number` magic constraint.
Implements sum.
-}
numberSum : CommutativeMonoid number
numberSum =
    { semigroup = Semigroup.numberSum, identity = 0, concat = List.sum }
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
    commutativeSemigroupAndIdentity Semigroup.and True


{-| Instance for any
-}
any : CommutativeMonoid Bool
any =
    commutativeSemigroupAndIdentity Semigroup.or False


{-| Instance for trivial monoid
-}
unit : CommutativeMonoid ()
unit =
    commutativeSemigroupAndIdentity Semigroup.unit ()


{-| Instance for exclusiveOr
-}
exclusiveOr : CommutativeMonoid Bool
exclusiveOr =
    commutativeSemigroupAndIdentity Semigroup.xor False


{-| Instance for modularArithmetic
-}
modularArithmetic : Int -> CommutativeMonoid Int
modularArithmetic divisor =
    commutativeSemigroupAndIdentity (Semigroup.modularArithmetic divisor) 0
