module SemigroupTests exposing (suite)

import CommutativeSemigroup
import Expect
import Fuzz
import Semigroup
import Set
import Test


suite : Test.Test
suite =
    Test.describe "The Semigroup abstraction"
        [ Test.describe "Commutative Semigroup"
            [ Test.fuzz3
                (Fuzz.intRange -100 100)
                (Fuzz.intRange -100 100)
                (Fuzz.intRange -100 100)
                "tests intProduct is associative"
              <|
                \a b c ->
                    let
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.intProduct

                        aTimesBThenTimesC =
                            semigroup (semigroup a b) c

                        bTimesCThenTimesA =
                            semigroup a (semigroup b c)
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
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.intProduct

                        aTimesB =
                            semigroup a b

                        bTimesA =
                            semigroup b a
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
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.numberSum

                        aPlusBThenPlusC =
                            semigroup (semigroup a b) c

                        bPlusCThenPlusA =
                            semigroup a (semigroup b c)
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
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.numberSum

                        aPlusB =
                            semigroup a b

                        bPlusA =
                            semigroup b a
                    in
                    aPlusB
                        |> Expect.equal bPlusA
            , Test.fuzz3
                Fuzz.bool
                Fuzz.bool
                Fuzz.bool
                "tests and is associative"
              <|
                \a b c ->
                    let
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.and

                        aAndBThenAndC =
                            semigroup (semigroup a b) c

                        bAndCThenAndA =
                            semigroup a (semigroup b c)
                    in
                    aAndBThenAndC
                        |> Expect.equal bAndCThenAndA
            , Test.fuzz2
                Fuzz.bool
                Fuzz.bool
                "tests and is commutative"
              <|
                \a b ->
                    let
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.and

                        aAndB =
                            semigroup a b

                        bAndA =
                            semigroup b a
                    in
                    aAndB
                        |> Expect.equal bAndA
            , Test.fuzz3
                Fuzz.bool
                Fuzz.bool
                Fuzz.bool
                "tests or is associative"
              <|
                \a b c ->
                    let
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.or

                        aOrBThenOrC =
                            semigroup (semigroup a b) c

                        bOrCThenOrA =
                            semigroup a (semigroup b c)
                    in
                    aOrBThenOrC
                        |> Expect.equal bOrCThenOrA
            , Test.fuzz2
                Fuzz.bool
                Fuzz.bool
                "tests or is commutative"
              <|
                \a b ->
                    let
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.or

                        aOrB =
                            semigroup a b

                        bOrA =
                            semigroup b a
                    in
                    aOrB
                        |> Expect.equal bOrA
            , Test.fuzz3
                Fuzz.unit
                Fuzz.unit
                Fuzz.unit
                "tests unit is associative"
              <|
                \a b c ->
                    let
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.unit

                        aOrBThenOrC =
                            semigroup (semigroup a b) c

                        bOrCThenOrA =
                            semigroup a (semigroup b c)
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
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.xor

                        aXorBThenXorC =
                            semigroup (semigroup a b) c

                        bXorCThenXorA =
                            semigroup a (semigroup b c)
                    in
                    aXorBThenXorC
                        |> Expect.equal bXorCThenXorA
            , Test.fuzz2
                Fuzz.bool
                Fuzz.bool
                "tests xor is commutative"
              <|
                \a b ->
                    let
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.xor

                        aXorB =
                            semigroup a b

                        bXorA =
                            semigroup b a
                    in
                    aXorB
                        |> Expect.equal bXorA
            , Test.fuzz3
                Fuzz.int
                Fuzz.int
                Fuzz.int
                "tests modularArithmetic is associative"
              <|
                \a b c ->
                    let
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.modularArithmetic 12

                        aPlusBThenPlusC =
                            semigroup (semigroup a b) c

                        bPlusCThenPlusA =
                            semigroup a (semigroup b c)
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
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.modularArithmetic 12

                        aPlusB =
                            semigroup a b

                        bPlusA =
                            semigroup b a
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
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.setUnion

                        aUnionBThenUnionC =
                            semigroup (semigroup a b) c

                        bUnionCThenUnionA =
                            semigroup a (semigroup b c)
                    in
                    aUnionBThenUnionC
                        |> Expect.equal bUnionCThenUnionA
            , Test.fuzz2
                (Fuzz.map Set.fromList (Fuzz.list Fuzz.int))
                (Fuzz.map Set.fromList (Fuzz.list Fuzz.int))
                "tests setUnion is commutative"
              <|
                \a b ->
                    let
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.setUnion

                        aSetUnionB =
                            semigroup a b

                        bSetUnionA =
                            semigroup b a
                    in
                    aSetUnionB
                        |> Expect.equal bSetUnionA
            , Test.fuzz3
                (Fuzz.map Set.fromList (Fuzz.list Fuzz.int))
                (Fuzz.map Set.fromList (Fuzz.list Fuzz.int))
                (Fuzz.map Set.fromList (Fuzz.list Fuzz.int))
                "tests setIntersection is associative"
              <|
                \a b c ->
                    let
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.setIntersection

                        aIntersectionBThenIntersectionC =
                            semigroup (semigroup a b) c

                        bIntersectionCThenIntersectionA =
                            semigroup a (semigroup b c)
                    in
                    aIntersectionBThenIntersectionC
                        |> Expect.equal bIntersectionCThenIntersectionA
            , Test.fuzz2
                (Fuzz.map Set.fromList (Fuzz.list Fuzz.int))
                (Fuzz.map Set.fromList (Fuzz.list Fuzz.int))
                "tests setIntersection is commutative"
              <|
                \a b ->
                    let
                        (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                            CommutativeSemigroup.setIntersection

                        aSetIntersectionB =
                            semigroup a b

                        bSetIntersectionA =
                            semigroup b a
                    in
                    aSetIntersectionB
                        |> Expect.equal bSetIntersectionA
            ]
        , Test.fuzz3
            Fuzz.string
            Fuzz.string
            Fuzz.string
            "tests string append is associative"
          <|
            \a b c ->
                let
                    aAppendBThenAppendC =
                        Semigroup.string (Semigroup.string a b) c

                    bAppendCThenAppendA =
                        Semigroup.string a (Semigroup.string b c)
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
                        Semigroup.maybeFirst (Semigroup.maybeFirst a b) c

                    bMaybeFirstCThenMaybeFirstA =
                        Semigroup.maybeFirst a (Semigroup.maybeFirst b c)
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
                        Semigroup.list (Semigroup.list a b) c

                    bAppendCThenAppendA =
                        Semigroup.list a (Semigroup.list b c)
                in
                aAppendBThenAppendC
                    |> Expect.equal bAppendCThenAppendA
        ]
