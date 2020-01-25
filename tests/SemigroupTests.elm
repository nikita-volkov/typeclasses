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
        ]
