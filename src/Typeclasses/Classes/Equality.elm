module Typeclasses.Classes.Equality exposing (Equality, eqAndNotEq, eq, compare, comparable, map, int, float, tuple2, tuple3, either)
{-|
Equality typeclass definition and its instances for basic types.

# Definition
@docs Equality

# Construction utilities
@docs eqAndNotEq, eq, compare, comparable

# Instance transformation utilities
@docs map

# Instances
@docs int, float, tuple2, tuple3, either

-}

import Either exposing (Either(..))


{-| Explicit typeclass which implements equality for type `a`. -}
type alias Equality a =
  {
    eq : a -> a -> Bool,
    notEq : a -> a -> Bool
  }


-- * Constructors
-------------------------

{-| Construct from the eq and not-eq operations. -}
eqAndNotEq : (a -> a -> Bool) -> (a -> a -> Bool) -> Equality a
eqAndNotEq eq_ notEq_ = { eq = eq_, notEq = notEq_ }

{-| Construct just from the eq operation. -}
eq : (a -> a -> Bool) -> Equality a
eq eq_ = { eq = eq_, notEq = \ l r -> not (eq_ l r) }

{-| Construct from a comparison operation. -}
compare : (a -> a -> Order) -> Equality a
compare compare_ =
  eq <| \ l r -> case compare_ l r of
    EQ -> True
    _ -> False

{-| Instance for any type, which satisfies the magic comparable constraint. -}
comparable : Equality comparable
comparable = eqAndNotEq (==) (/=)


-- * Transformations
-------------------------

{-| Map over the owner type of an instance to produce a new instance.

Please notice that mapping is contravariant (i.e., `(b -> a)` instead of `(a -> b)`).
-}
map : (b -> a) -> Equality a -> Equality b
map bToA equalityOfA = eq (\ lb rb -> equalityOfA.eq (bToA lb) (bToA rb))


-- * Instances
-------------------------

{-| Instance for Int. -}
int : Equality Int
int = eqAndNotEq (==) (/=)

{-| Instance for Float. -}
float : Equality Float
float = eqAndNotEq (==) (/=)

{-| Instance for tuple, with instances for its members provided. -}
tuple2 : Equality a -> Equality b -> Equality (a, b)
tuple2 equalityOfA equalityOfB =
  eq <| \ (la, lb) (ra, rb) ->
    equalityOfA.eq la ra &&
    equalityOfB.eq lb rb

{-| Instance for tuple, with instances for its members provided. -}
tuple3 : Equality a -> Equality b -> Equality c -> Equality (a, b, c)
tuple3 equalityOfA equalityOfB equalityOfC =
  eq <| \ (la, lb, lc) (ra, rb, rc) ->
    equalityOfA.eq la ra &&
    equalityOfB.eq lb rb &&
    equalityOfC.eq lc rc

{-| Instance for Either, with instances for its members provided. -}
either : Equality a -> Equality b -> Equality (Either a b)
either equalityOfA equalityOfB =
  eq <| \ le re ->
    case le of
      Left la -> case re of
        Left ra -> equalityOfA.eq la ra
        _ -> False
      Right lb -> case re of
        Right rb -> equalityOfB.eq lb rb
        _ -> False
