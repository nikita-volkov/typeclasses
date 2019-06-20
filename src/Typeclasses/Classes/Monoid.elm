module Typeclasses.Classes.Monoid exposing (..)

import Typeclasses.Classes.Semigroup as Semigroup exposing (Semigroup)
import Either exposing (Either(..))
import Set exposing (Set)

{-| Explicit typeclass which implements monoid operations for type `a`. -}
type alias Monoid a =
  {
    semigroup : Semigroup a,
    identity : a,
    concat : List a -> a
  }

identityAndConcat : a -> (List a -> a) -> Monoid a
identityAndConcat identity concat =
  {
    semigroup = Semigroup.concat concat,
    identity = identity,
    concat = concat
  }

semigroupAndIdentity : Semigroup a -> a -> Monoid a
semigroupAndIdentity semigroup identity =
  {
    semigroup = semigroup,
    identity = identity,
    concat = List.foldl semigroup.prepend identity
  }

appendable : appendable -> Monoid appendable
appendable = semigroupAndIdentity Semigroup.appendable

numberProduct : Monoid number
numberProduct = { semigroup = Semigroup.numberProduct, identity = 1, concat = List.product }

numberSum : Monoid number
numberSum = { semigroup = Semigroup.numberSum, identity = 0, concat = List.sum }

intProduct : Monoid Int
intProduct = numberProduct

intSum : Monoid Int
intSum = numberSum

string : Monoid String
string = appendable ""

list : Monoid (List a)
list = appendable []

setUnion : Monoid (Set comparable)
setUnion = semigroupAndIdentity Semigroup.setUnion Set.empty

setIntersection : Monoid (Set comparable)
setIntersection = semigroupAndIdentity Semigroup.setIntersection Set.empty

setDifference : Monoid (Set comparable)
setDifference = semigroupAndIdentity Semigroup.setDifference Set.empty

{-| Map over the owner type of an instance to produce a new instance.

You need to provide both a covariant and a contravariant mapping
(i.e., `(a -> b)` and `(b -> a)`).
-}
map : (a -> b) -> (b -> a) -> Monoid a -> Monoid b
map aToB bToA monoidOfA =
  semigroupAndIdentity
    (Semigroup.map aToB bToA monoidOfA.semigroup)
    (aToB monoidOfA.identity)
