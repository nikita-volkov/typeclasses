module Typeclasses.Semigroup exposing (..)

import Either exposing (Either(..))
import Set exposing (Set)

{-| Explicit typeclass which implements semigroup operations for type `a`. -}
type alias Semigroup a =
  {
    prepend : a -> a -> a
  }

prepend : (a -> a -> a) -> Semigroup a
prepend prepend_ = { prepend = prepend_ }

appendable : Semigroup appendable
appendable = { prepend = (++) }

numberProduct : Semigroup number
numberProduct = { prepend = (*) }

numberSum : Semigroup number
numberSum = { prepend = (+) }

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
