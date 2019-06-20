module Typeclasses.Equality exposing (..)

import Either exposing (Either(..))

{-| Explicit typeclass which implements equality for type `a`. -}
type alias Equality a =
  {
    eq : a -> a -> Bool
  }

eq : (a -> a -> Bool) -> Equality a
eq eq_ = { eq = eq_ }

compare : (a -> a -> Order) -> Equality a
compare compare_ =
  eq <| \ l r -> case compare_ l r of
    EQ -> True
    _ -> False

comparable : Equality comparable
comparable = compare Basics.compare

int : Equality Int
int = eq (==)

float : Equality Float
float = eq (==)

tuple2 : Equality a -> Equality b -> Equality (a, b)
tuple2 equalityOfA equalityOfB =
  eq <| \ (la, lb) (ra, rb) ->
    equalityOfA.eq la ra &&
    equalityOfB.eq lb rb

tuple3 : Equality a -> Equality b -> Equality c -> Equality (a, b, c)
tuple3 equalityOfA equalityOfB equalityOfC =
  eq <| \ (la, lb, lc) (ra, rb, rc) ->
    equalityOfA.eq la ra &&
    equalityOfB.eq lb rb &&
    equalityOfC.eq lc rc

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

{-| Map over the owner type of an instance to produce a new instance.

Please notice that mapping is contravariant (i.e., `(b -> a)` instead of `(a -> b)`).
-}
map : (b -> a) -> Equality a -> Equality b
map bToA equalityOfA = eq (\ lb rb -> equalityOfA.eq (bToA lb) (bToA rb))
