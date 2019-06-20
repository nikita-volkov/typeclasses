module Typeclasses.Classes.Semigroup exposing (..)

import Either exposing (Either(..))
import Set exposing (Set)
import Task exposing (Task)

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

maybeFirst : Semigroup (Maybe a)
maybeFirst = prepend <| \ l r -> case l of
  Nothing -> r
  _ -> l

list : Semigroup (List a)
list = appendable

setUnion : Semigroup (Set comparable)
setUnion = prepend Set.union

setIntersection : Semigroup (Set comparable)
setIntersection = prepend Set.intersect

setDifference : Semigroup (Set comparable)
setDifference = prepend Set.diff

cmd : Semigroup (Cmd msg)
cmd = concat Cmd.batch

sub : Semigroup (Sub msg)
sub = concat Sub.batch

task : Semigroup a -> Semigroup (Task x a)
task semigroupOfA = prepend <| \ l r -> l |> Task.andThen (\ la -> Task.map (semigroupOfA.prepend la) r)

{-| Map over the owner type of an instance to produce a new instance.

You need to provide both a covariant and a contravariant mapping
(i.e., `(a -> b)` and `(b -> a)`).
-}
map : (a -> b) -> (b -> a) -> Semigroup a -> Semigroup b
map aToB bToA semigroupOfA = prepend (\ lb rb -> aToB (semigroupOfA.prepend (bToA lb) (bToA rb)))
