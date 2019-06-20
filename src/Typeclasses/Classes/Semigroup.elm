module Typeclasses.Classes.Semigroup exposing (..)

import Either exposing (Either(..))
import Set exposing (Set)

{-| Explicit typeclass which implements semigroup operations for type `a`. -}
type alias Semigroup a =
  {
    prepend : a -> a -> a
  }

prepend : (a -> a -> a) -> Semigroup a
prepend prepend_ = { prepend = prepend_ }

concat : (List a -> a) -> Semigroup a
concat concat_ = prepend (\ l r -> concat_ [l, r])

appendable : Semigroup appendable
appendable = prepend (++)

numberProduct : Semigroup number
numberProduct = prepend (*)

numberSum : Semigroup number
numberSum = prepend (+)

intProduct : Semigroup Int
intProduct = numberProduct

intSum : Semigroup Int
intSum = numberSum

string : Semigroup String
string = appendable

list : Semigroup (List a)
list = appendable

setUnion : Semigroup (Set comparable)
setUnion = prepend Set.union

setIntersection : Semigroup (Set comparable)
setIntersection = prepend Set.intersect

setDifference : Semigroup (Set comparable)
setDifference = prepend Set.diff

{-| Map over the owner type of an instance to produce a new instance.

You need to provide both a covariant and a contravariant mapping
(i.e., `(a -> b)` and `(b -> a)`).
-}
map : (a -> b) -> (b -> a) -> Semigroup a -> Semigroup b
map aToB bToA semigroupOfA = prepend (\ lb rb -> aToB (semigroupOfA.prepend (bToA lb) (bToA rb)))
