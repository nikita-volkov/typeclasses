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
                    inversePlusA =
                        Group.numberSum.monoid.semigroup
                            (Group.numberSum.inverse a)
                            a

                    aPlusInverse =
                        Group.numberSum.monoid.semigroup
                            a
                            (Group.numberSum.inverse a)
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
                    inversePlusA =
                        Group.trivial.monoid.semigroup
                            (Group.trivial.inverse a)
                            a

                    aPlusInverse =
                        Group.trivial.monoid.semigroup
                            a
                            (Group.trivial.inverse a)
                in
                Expect.true "All equal a"
                    (inversePlusA
                        == Group.trivial.monoid.identity
                        && aPlusInverse
                        == Group.trivial.monoid.identity
                    )
        , Test.fuzz
            Fuzz.bool
            "tests exclusiveOr has an inverse"
          <|
            \a ->
                let
                    inverseXorA =
                        Group.exclusiveOr.monoid.semigroup
                            (Group.exclusiveOr.inverse a)
                            a

                    aXorInverse =
                        Group.exclusiveOr.monoid.semigroup
                            a
                            (Group.exclusiveOr.inverse a)
                in
                Expect.true "All equal a"
                    (inverseXorA
                        == Group.exclusiveOr.monoid.identity
                        && aXorInverse
                        == Group.exclusiveOr.monoid.identity
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
