module Typeclasses.Hashing exposing (..)
{-| Hashing implementation much inspired by
[the "hashable" Haskell library](http://hackage.haskell.org/package/hashable).
-}

import Typeclasses.Hashing.Int as Int
import Either exposing (Either(..))
import Array exposing (Array)

{-| Explicit typeclass which implements hashing for type `a`. -}
type alias Hashing a =
  {
    hash : a -> Int,
    hashWithSalt : Int -> a -> Int
  }

hash : (a -> Int) -> Hashing a
hash hash_ =
  {
    hash = hash_,
    hashWithSalt = \ salt a -> Int.combine salt (hash_ a)
  }

int : Hashing Int
int =
  {
    hash = identity,
    hashWithSalt = Int.combine
  }

list : Hashing a -> Hashing (List a)
list hashingOfA = hash <| \ listOfA ->
  List.foldl (\ a hash_ -> hashingOfA.hashWithSalt hash_ a) (List.length listOfA) listOfA

{-| Hashing instance for arrays,
determined by the Hashing instance for elements and sampling.
Sampling determines the maximum amount of elements to be picked
from the array while constructing the hash,
allowing you to control the balance between the quality of hashing and its performance.
-}
array : Hashing a -> Int -> Hashing (Array a)
array hashingOfA sampling = Debug.todo ""

{-| Hashing instance for strings, determined by sampling.
Sampling determines the maximum amount of characters to be picked
from the string while constructing the hash,
allowing you to control the balance between the quality of hashing and its performance.
-}
string : Int -> Hashing String
string sampling = Debug.todo ""
