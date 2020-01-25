module SemigroupTests exposing (suite)

import Expect
import Fuzz
import Test
import Typeclasses.Classes.Semigroup


suite : Test.Test
suite =
    Test.describe "The Semigroup abstraction"
        [ Test.fuzz3
            (Fuzz.intRange -100 100)
            (Fuzz.intRange -100 100)
            (Fuzz.intRange -100 100)
            "tests intProduct is associative"
          <|
            \a b c ->
                let
                    aTimesBThenTimesC =
                        Typeclasses.Classes.Semigroup.intProduct.prepend (Typeclasses.Classes.Semigroup.intProduct.prepend a b) c

                    bTimesCThenTimesA =
                        Typeclasses.Classes.Semigroup.intProduct.prepend a (Typeclasses.Classes.Semigroup.intProduct.prepend b c)
                in
                aTimesBThenTimesC
                    |> Expect.equal bTimesCThenTimesA
        , Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "tests numberSum is associative"
          <|
            \a b c ->
                let
                    aPlusBThenPlusC =
                        Typeclasses.Classes.Semigroup.numberSum.prepend (Typeclasses.Classes.Semigroup.numberSum.prepend a b) c

                    bPlusCThenPlusA =
                        Typeclasses.Classes.Semigroup.numberSum.prepend a (Typeclasses.Classes.Semigroup.numberSum.prepend b c)
                in
                aPlusBThenPlusC
                    |> Expect.equal bPlusCThenPlusA
        , Test.fuzz3
            Fuzz.string
            Fuzz.string
            Fuzz.string
            "tests string append is associative"
          <|
            \a b c ->
                let
                    aAppendBThenAppendC =
                        Typeclasses.Classes.Semigroup.string.prepend (Typeclasses.Classes.Semigroup.string.prepend a b) c

                    bAppendCThenAppendA =
                        Typeclasses.Classes.Semigroup.string.prepend a (Typeclasses.Classes.Semigroup.string.prepend b c)
                in
                aAppendBThenAppendC
                    |> Expect.equal bAppendCThenAppendA
        , Test.fuzz3
            (Fuzz.maybe Fuzz.unit)
            (Fuzz.maybe Fuzz.unit)
            (Fuzz.maybe Fuzz.unit)
            "tests maybeFirst is associative"
          <|
            \a b c ->
                let
                    aMaybeFirstBThenMaybeFirstC =
                        Typeclasses.Classes.Semigroup.maybeFirst.prepend (Typeclasses.Classes.Semigroup.maybeFirst.prepend a b) c

                    bMaybeFirstCThenMaybeFirstA =
                        Typeclasses.Classes.Semigroup.maybeFirst.prepend a (Typeclasses.Classes.Semigroup.maybeFirst.prepend b c)
                in
                aMaybeFirstBThenMaybeFirstC
                    |> Expect.equal bMaybeFirstCThenMaybeFirstA
        ]
