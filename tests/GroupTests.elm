module GroupTests exposing (suite)

import AbelianGroup
import CommutativeMonoid
import CommutativeSemigroup
import Expect
import Fuzz
import Group
import Test


suite : Test.Test
suite =
    Test.describe "The Group abstraction"
        [ Test.fuzz
            Fuzz.int
            "tests numberSum has an inverse"
          <|
            \a ->
                let
                    group =
                        Group.numberSum

                    inversePlusA =
                        Group.numberSum.monoid.semigroup
                            (group.inverse a)
                            a

                    aPlusInverse =
                        Group.numberSum.monoid.semigroup
                            a
                            (group.inverse a)
                in
                Expect.true "All equal a"
                    (inversePlusA
                        == Group.numberSum.monoid.identity
                        && aPlusInverse
                        == Group.numberSum.monoid.identity
                    )
        , Test.fuzz
            Fuzz.unit
            "tests trivial group has an inverse"
          <|
            \a ->
                let
                    group =
                        Group.trivialGroup

                    inversePlusA =
                        group.monoid.semigroup
                            (group.inverse a)
                            a

                    aPlusInverse =
                        group.monoid.semigroup
                            a
                            (group.inverse a)
                in
                Expect.true "All equal a"
                    (inversePlusA
                        == group.monoid.identity
                        && aPlusInverse
                        == group.monoid.identity
                    )
        , Test.fuzz
            Fuzz.bool
            "tests exclusiveOr has an inverse"
          <|
            \a ->
                let
                    group =
                        Group.exclusiveOr

                    inverseXorA =
                        group.monoid.semigroup
                            (group.inverse a)
                            a

                    aXorInverse =
                        group.monoid.semigroup
                            a
                            (group.inverse a)
                in
                Expect.true "All equal a"
                    (inverseXorA
                        == group.monoid.identity
                        && aXorInverse
                        == group.monoid.identity
                    )
        , Test.fuzz
            Fuzz.int
            "tests modularArithmetic has an inverse"
          <|
            \a ->
                let
                    group =
                        Group.modularArithmetic 12

                    inversePlusA =
                        group.monoid.semigroup
                            (group.inverse a)
                            a

                    aPlusInverse =
                        group.monoid.semigroup
                            a
                            (group.inverse a)
                in
                Expect.true "All equal a"
                    (inversePlusA
                        == group.monoid.identity
                        && aPlusInverse
                        == group.monoid.identity
                    )
        ]
