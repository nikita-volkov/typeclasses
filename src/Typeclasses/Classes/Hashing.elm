module Typeclasses.Classes.Hashing exposing (Hashing, hash, hashWithSalt, map, int, list, array, string)
{-|
Hashing typeclass definition and its instances for basic types.
Much inspired by [the "hashable" Haskell library](http://hackage.haskell.org/package/hashable).

# Definition
@docs Hashing

# Construction utilities
@docs hash, hashWithSalt

# Instance transformation utilities
@docs map

# Instances
@docs int, list, array, string

-}

import Typeclasses.Classes.Hashing.Hash as Hash
import Either exposing (Either(..))
import Array exposing (Array)


{-| Explicit typeclass which implements hashing for type `a`. -}
type alias Hashing a =
  {
    hash : a -> Int,
    hashWithSalt : Int -> a -> Int
  }


-- * Constructors
-------------------------

{-| Construct from just the hashing function. -}
hash : (a -> Int) -> Hashing a
hash hash_ =
  {
    hash = hash_,
    hashWithSalt = \ salt a -> Hash.intWithSalt salt (hash_ a)
  }

{-| Construct from just the hashing function, which combines the hash with salt. -}
hashWithSalt : (Int -> a -> Int) -> Hashing a
hashWithSalt hashWithSalt_ =
  {
    hash = hashWithSalt_ 0,
    hashWithSalt = hashWithSalt_
  }


-- * Transformations
-------------------------

{-| Map over the owner type of an instance to produce a new instance.

Please notice that mapping is contravariant (i.e., `(b -> a)` instead of `(a -> b)`).
-}
map : (b -> a) -> Hashing a -> Hashing b
map bToA hashingOfA =
  {
    hash = \ b -> hashingOfA.hash (bToA b)
    ,
    hashWithSalt = \ salt b -> hashingOfA.hashWithSalt salt (bToA b)
  }


-- * Instances
-------------------------

{-| Instance for `Int`. -}
int : Hashing Int
int =
  {
    hash = identity,
    hashWithSalt = Hash.intWithSalt
  }

{-| Instance for `List`, which utilizes an instance for its element.
Traverses the list in whole. -}
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
array hashingOfA sampling = hashWithSalt <| \ salt arrayOfA ->
  let
    length = Array.length arrayOfA
    in if length >= sampling
      then
        let
          chunkLength = length // sampling
          indexInChunk = modBy chunkLength salt
          loop iterationIndex collectedHash =
            if iterationIndex < sampling
              then case Array.get (iterationIndex * chunkLength + indexInChunk) arrayOfA of
                Just a -> loop (iterationIndex + 1) (hashingOfA.hashWithSalt collectedHash a)
                Nothing -> collectedHash
              else collectedHash
          in loop 0 (Hash.intWithSalt salt (Hash.intWithSalt sampling length))
      else Array.foldl (\ a collectedHash -> hashingOfA.hashWithSalt collectedHash a) (Hash.intWithSalt salt length) arrayOfA

{-| Hashing instance for strings, determined by sampling.
Sampling determines the maximum amount of characters to be picked
from the string while constructing the hash,
allowing you to control the balance between the quality of hashing and its performance.
-}
string : Int -> Hashing String
string sampling = hashWithSalt <| \ salt x ->
  let
    length = String.length x
    in if length >= sampling
      then
        let
          chunkLength = length // sampling
          indexInChunk = modBy chunkLength salt
          loop iterationIndex collectedHash =
            if iterationIndex < sampling
              then
                let
                  startIndex = iterationIndex * chunkLength + indexInChunk
                  endIndex = startIndex + 1
                  slice = String.slice startIndex endIndex x
                  in case String.toList slice of
                    c :: _ -> loop (iterationIndex + 1) (Hash.charWithSalt salt c)
                    _ -> collectedHash
              else collectedHash
          in loop 0 (Hash.intWithSalt salt (Hash.intWithSalt sampling length))
      else String.foldl (\ c collectedHash -> Hash.charWithSalt collectedHash c) (Hash.intWithSalt salt length) x
