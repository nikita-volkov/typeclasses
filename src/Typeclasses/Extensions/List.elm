module Typeclasses.Extensions.List exposing (fold, foldMap, sort, sortBy, member, dedupe, remove)

{-| Extensions to the list API, which utilise typeclasses.

@docs fold, foldMap, sort, sortBy, member, dedupe, remove

-}

import Typeclasses.Classes.Comparison as Comparison exposing (Comparison)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Monoid as Monoid exposing (Monoid)


{-| _O(n)_. Combine the elements of list using a `Monoid` instance.
-}
fold : Monoid a -> List a -> a
fold monoid =
    monoid.concat


{-| _O(n)_. Map each element of the list to a type which has an instance of `Monoid`,
and combine the results.
-}
foldMap : Monoid b -> (a -> b) -> List a -> b
foldMap monoidOfB aToB =
    List.foldl (monoidOfB.semigroup << aToB) monoidOfB.identity


{-| Sort the elements using a `Comparison` instance.
-}
sort : Comparison a -> List a -> List a
sort comparison =
    List.sortWith comparison.compare


{-| Sort the elements by mapping to a type which has an instance of `Comparison`.
-}
sortBy : Comparison b -> (a -> b) -> List a -> List a
sortBy comparisonOfB aToB =
    List.sortWith (\la ra -> comparisonOfB.compare (aToB la) (aToB ra))


{-| _O(n)_. Check whether a value is a member of a list based on the value's `Equality` instance.
-}
member : Equality a -> a -> List a -> Bool
member equality a =
    let
        loop list =
            case list of
                head :: tail ->
                    if equality.eq a head then
                        True

                    else
                        loop tail

                _ ->
                    False
    in
    loop


{-| _O(n^2)_. Deduplicate a list using an instance of `Equality`.
-}
dedupe : Equality a -> List a -> List a
dedupe equality =
    List.foldr
        (\a newList ->
            if member equality a newList then
                newList

            else
                a :: newList
        )
        []


{-| _O(n)_. Remove the first occurrence of equaling element.
-}
remove : Equality a -> a -> List a -> List a
remove equality a list =
    let
        eq =
            equality.eq

        loop precedingList currentList =
            case currentList of
                head :: tail ->
                    if eq a head then
                        prependReversed precedingList tail

                    else
                        loop (head :: precedingList) tail

                _ ->
                    list
    in
    loop [] list


prependReversed : List a -> List a -> List a
prependReversed left =
    case left of
        head :: tail ->
            prependReversed tail << (::) head

        _ ->
            identity
