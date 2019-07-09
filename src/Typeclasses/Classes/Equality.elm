module Typeclasses.Classes.Equality exposing (Equality, eqAndNotEq, eq, compare, comparable, map, bool, int, float, char, string, maybe, result, either, list, array, tuple2, tuple3)
{-|
Equality typeclass definition and its instances for basic types.

# Definition
@docs Equality

# Construction utilities
@docs eqAndNotEq, eq, compare, comparable

# Instance transformation utilities
@docs map

# Instances

## Primitives

@docs bool, int, float, char, string

## Composites

@docs maybe, result, either, list, array

## Tuples

@docs tuple2, tuple3

-}

import Array exposing (Array)
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


-- ** Primitives
-------------------------

{-| Instance for `Bool`. -}
bool : Equality Bool
bool = eqAndNotEq (==) (/=)

{-| Instance for `Int`. -}
int : Equality Int
int = eqAndNotEq (==) (/=)

{-| Instance for `Float`. -}
float : Equality Float
float = eqAndNotEq (==) (/=)

{-| Instance for `Char`. -}
char : Equality Char
char = eqAndNotEq (==) (/=)

{-| Instance for `String`. -}
string : Equality String
string = eqAndNotEq (==) (/=)


-- ** Composites
-------------------------

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

{-| Equality for `Maybe`, with instance for its member provided. -}
maybe : Equality a -> Equality (Maybe a)
maybe memberEquality =
  eq <| \ lMaybe rMaybe -> case lMaybe of
    Just l -> case rMaybe of
      Just r -> memberEquality.eq l r
      _ -> False
    Nothing -> case rMaybe of
      Nothing -> True
      _ -> False

{-| Instance for `Result`, with instances for its members provided. -}
result : Equality a -> Equality b -> Equality (Result a b)
result equalityOfA equalityOfB =
  eq <| \ le re ->
    case le of
      Ok lb -> case re of
        Ok rb -> equalityOfB.eq lb rb
        _ -> False
      Err la -> case re of
        Err ra -> equalityOfA.eq la ra
        _ -> False

{-| Instance for `Either`, with instances for its members provided. -}
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

{-| Equality for `List`, with instance for its member provided. -}
list : Equality a -> Equality (List a)
list memberEquality =
  eq <|
  let
    loop lList rList = case lList of
      lHead :: lTail -> case rList of
        rHead :: rTail -> if memberEquality.eq lHead rHead
          then loop lTail rTail
          else False
        _ -> False
      [] -> case rList of
        [] -> True
        _ -> False
    in loop

{-| Equality for `Array`, with instance for its member provided. -}
array : Equality a -> Equality (Array a)
array memberEquality =
  eq <| \ lArray rArray ->
  if Array.length lArray == Array.length rArray
    then (list memberEquality).eq (Array.toList lArray) (Array.toList rArray)
    else False
