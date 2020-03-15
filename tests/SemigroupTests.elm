module SemigroupTests exposing (suite)

import Expect
import Fuzz
import Set
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
                    (Typeclasses.Classes.Semigroup.CommutativeSemigroup semigroup) =
                        Typeclasses.Classes.Semigroup.intProduct

                    aTimesBThenTimesC =
                        semigroup.prepend (semigroup.prepend a b) c

                    bTimesCThenTimesA =
                        semigroup.prepend a (semigroup.prepend b c)
                in
                aTimesBThenTimesC
                    |> Expect.equal bTimesCThenTimesA
        , Test.fuzz2
            (Fuzz.intRange -100 100)
            (Fuzz.intRange -100 100)
            "tests intProduct is commutative"
          <|
            \a b ->
                let
                    (Typeclasses.Classes.Semigroup.CommutativeSemigroup semigroup) =
                        Typeclasses.Classes.Semigroup.intProduct

                    aTimesB =
                        semigroup.prepend a b

                    bTimesA =
                        semigroup.prepend b a
                in
                aTimesB
                    |> Expect.equal bTimesA
        , Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "tests numberSum is associative"
          <|
            \a b c ->
                let
                    (Typeclasses.Classes.Semigroup.CommutativeSemigroup semigroup) =
                        Typeclasses.Classes.Semigroup.intSum

                    aPlusBThenPlusC =
                        semigroup.prepend (semigroup.prepend a b) c

                    bPlusCThenPlusA =
                        semigroup.prepend a (semigroup.prepend b c)
                in
                aPlusBThenPlusC
                    |> Expect.equal bPlusCThenPlusA
        , Test.fuzz2
            (Fuzz.intRange -100 100)
            (Fuzz.intRange -100 100)
            "tests numberSum is commutative"
          <|
            \a b ->
                let
                    (Typeclasses.Classes.Semigroup.CommutativeSemigroup semigroup) =
                        Typeclasses.Classes.Semigroup.numberSum

                    aPlusB =
                        semigroup.prepend a b

                    bPlusA =
                        semigroup.prepend b a
                in
                aPlusB
                    |> Expect.equal bPlusA
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
        , Test.fuzz3
            (Fuzz.list Fuzz.unit)
            (Fuzz.list Fuzz.unit)
            (Fuzz.list Fuzz.unit)
            "tests list is associative"
          <|
            \a b c ->
                let
                    aAppendBThenAppendC =
                        Typeclasses.Classes.Semigroup.list.prepend (Typeclasses.Classes.Semigroup.list.prepend a b) c

                    bAppendCThenAppendA =
                        Typeclasses.Classes.Semigroup.list.prepend a (Typeclasses.Classes.Semigroup.list.prepend b c)
                in
                aAppendBThenAppendC
                    |> Expect.equal bAppendCThenAppendA
        , Test.fuzz3
            Fuzz.bool
            Fuzz.bool
            Fuzz.bool
            "tests and is associative"
          <|
            \a b c ->
                let
                    aAndBThenAndC =
                        Typeclasses.Classes.Semigroup.and.prepend (Typeclasses.Classes.Semigroup.and.prepend a b) c

                    bAndCThenAndA =
                        Typeclasses.Classes.Semigroup.and.prepend a (Typeclasses.Classes.Semigroup.and.prepend b c)
                in
                aAndBThenAndC
                    |> Expect.equal bAndCThenAndA
        , Test.fuzz3
            Fuzz.bool
            Fuzz.bool
            Fuzz.bool
            "tests or is associative"
          <|
            \a b c ->
                let
                    aOrBThenOrC =
                        Typeclasses.Classes.Semigroup.or.prepend (Typeclasses.Classes.Semigroup.or.prepend a b) c

                    bOrCThenOrA =
                        Typeclasses.Classes.Semigroup.or.prepend a (Typeclasses.Classes.Semigroup.or.prepend b c)
                in
                aOrBThenOrC
                    |> Expect.equal bOrCThenOrA
        , Test.fuzz3
            Fuzz.unit
            Fuzz.unit
            Fuzz.unit
            "tests unit is associative"
          <|
            \a b c ->
                let
                    aOrBThenOrC =
                        Typeclasses.Classes.Semigroup.unit.prepend (Typeclasses.Classes.Semigroup.unit.prepend a b) c

                    bOrCThenOrA =
                        Typeclasses.Classes.Semigroup.unit.prepend a (Typeclasses.Classes.Semigroup.unit.prepend b c)
                in
                aOrBThenOrC
                    |> Expect.equal bOrCThenOrA
        , Test.fuzz3
            Fuzz.bool
            Fuzz.bool
            Fuzz.bool
            "tests xor is associative"
          <|
            \a b c ->
                let
                    aOrBThenOrC =
                        Typeclasses.Classes.Semigroup.xor.prepend (Typeclasses.Classes.Semigroup.xor.prepend a b) c

                    bOrCThenOrA =
                        Typeclasses.Classes.Semigroup.xor.prepend a (Typeclasses.Classes.Semigroup.xor.prepend b c)
                in
                aOrBThenOrC
                    |> Expect.equal bOrCThenOrA
        , Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "tests modularArithmetic is associative"
          <|
            \a b c ->
                let
                    (Typeclasses.Classes.Semigroup.CommutativeSemigroup semigroup) =
                        Typeclasses.Classes.Semigroup.modularArithmetic 12

                    aPlusBThenPlusC =
                        semigroup.prepend (semigroup.prepend a b) c

                    bPlusCThenPlusA =
                        semigroup.prepend a (semigroup.prepend b c)
                in
                aPlusBThenPlusC
                    |> Expect.equal bPlusCThenPlusA
        , Test.fuzz2
            (Fuzz.intRange -100 100)
            (Fuzz.intRange -100 100)
            "tests modularArithmetic is commutative"
          <|
            \a b ->
                let
                    (Typeclasses.Classes.Semigroup.CommutativeSemigroup semigroup) =
                        Typeclasses.Classes.Semigroup.modularArithmetic 12

                    aPlusB =
                        semigroup.prepend a b

                    bPlusA =
                        semigroup.prepend b a
                in
                aPlusB
                    |> Expect.equal bPlusA
        , Test.fuzz3
            (Fuzz.map Set.fromList (Fuzz.list Fuzz.int))
            (Fuzz.map Set.fromList (Fuzz.list Fuzz.int))
            (Fuzz.map Set.fromList (Fuzz.list Fuzz.int))
            "tests setUnion is associative"
          <|
            \a b c ->
                let
                    aUnionBThenUnionC =
                        Typeclasses.Classes.Semigroup.setUnion.prepend (Typeclasses.Classes.Semigroup.setUnion.prepend a b) c

                    bUnionCThenUnionA =
                        Typeclasses.Classes.Semigroup.setUnion.prepend a (Typeclasses.Classes.Semigroup.setUnion.prepend b c)
                in
                aUnionBThenUnionC
                    |> Expect.equal bUnionCThenUnionA
        ]
