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
tuple2 equalityA equalityB =
  eq <| \ (la, lb) (ra, rb) ->
    equalityA.eq la ra &&
    equalityB.eq lb rb

tuple3 : Equality a -> Equality b -> Equality c -> Equality (a, b, c)
tuple3 equalityA equalityB equalityC =
  eq <| \ (la, lb, lc) (ra, rb, rc) ->
    equalityA.eq la ra &&
    equalityB.eq lb rb &&
    equalityC.eq lc rc

either : Equality a -> Equality b -> Equality (Either a b)
either equalityA equalityB =
  eq <| \ le re ->
    case le of
      Left la -> case re of
        Left ra -> equalityA.eq la ra
        _ -> False
      Right lb -> case re of
        Right rb -> equalityB.eq lb rb
        _ -> False
