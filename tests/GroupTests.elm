module GroupTests exposing (suite)

import AbelianGroup
import CommutativeMonoid
import CommutativeSemigroup
import Expect
import Fuzz
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
                    (AbelianGroup.AbelianGroup group) =
                        AbelianGroup.numberSum

                    (CommutativeMonoid.CommutativeMonoid monoid) =
                        group.monoid

                    (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                        monoid.semigroup

                    inversePlusA =
                        semigroup
                            (group.inverse a)
                            a

                    aPlusInverse =
                        semigroup
                            a
                            (group.inverse a)
                in
                Expect.true "All equal a"
                    (inversePlusA
                        == monoid.identity
                        && aPlusInverse
                        == monoid.identity
                    )
        , Test.fuzz
            Fuzz.unit
            "tests trivial group has an inverse"
          <|
            \a ->
                let
                    (AbelianGroup.AbelianGroup group) =
                        AbelianGroup.trivialGroup

                    (CommutativeMonoid.CommutativeMonoid monoid) =
                        group.monoid

                    (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                        monoid.semigroup

                    inversePlusA =
                        semigroup
                            (group.inverse a)
                            a

                    aPlusInverse =
                        semigroup
                            a
                            (group.inverse a)
                in
                Expect.true "All equal a"
                    (inversePlusA
                        == monoid.identity
                        && aPlusInverse
                        == monoid.identity
                    )
        , Test.fuzz
            Fuzz.bool
            "tests exclusiveOr has an inverse"
          <|
            \a ->
                let
                    (AbelianGroup.AbelianGroup group) =
                        AbelianGroup.exclusiveOr

                    (CommutativeMonoid.CommutativeMonoid monoid) =
                        group.monoid

                    (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                        monoid.semigroup

                    inverseXorA =
                        semigroup
                            (group.inverse a)
                            a

                    aXorInverse =
                        semigroup
                            a
                            (group.inverse a)
                in
                Expect.true "All equal a"
                    (inverseXorA
                        == monoid.identity
                        && aXorInverse
                        == monoid.identity
                    )
        , Test.fuzz
            Fuzz.int
            "tests modularArithmetic has an inverse"
          <|
            \a ->
                let
                    (AbelianGroup.AbelianGroup group) =
                        AbelianGroup.modularArithmetic 12

                    (CommutativeMonoid.CommutativeMonoid monoid) =
                        group.monoid

                    (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                        monoid.semigroup

                    inversePlusA =
                        semigroup
                            (group.inverse a)
                            a

                    aPlusInverse =
                        semigroup
                            a
                            (group.inverse a)
                in
                Expect.true "All equal a"
                    (inversePlusA
                        == monoid.identity
                        && aPlusInverse
                        == monoid.identity
                    )
        ]
