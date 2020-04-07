module GroupTests exposing (suite)

import Expect
import Fuzz
import Test
import Typeclasses.Classes.AbelianGroup
import Typeclasses.Classes.CommutativeMonoid
import Typeclasses.Classes.CommutativeSemigroup


suite : Test.Test
suite =
    Test.describe "The Group abstraction"
        [ Test.fuzz
            Fuzz.int
            "tests numberSum has an inverse"
          <|
            \a ->
                let
                    (Typeclasses.Classes.AbelianGroup.AbelianGroup group) =
                        Typeclasses.Classes.AbelianGroup.numberSum

                    (Typeclasses.Classes.CommutativeMonoid.CommutativeMonoid monoid) =
                        group.monoid

                    (Typeclasses.Classes.CommutativeSemigroup.CommutativeSemigroup semigroup) =
                        monoid.semigroup

                    inversePlusA =
                        semigroup.prepend
                            (group.inverse a)
                            a

                    aPlusInverse =
                        semigroup.prepend
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
                    (Typeclasses.Classes.AbelianGroup.AbelianGroup group) =
                        Typeclasses.Classes.AbelianGroup.trivialGroup

                    (Typeclasses.Classes.CommutativeMonoid.CommutativeMonoid monoid) =
                        group.monoid

                    (Typeclasses.Classes.CommutativeSemigroup.CommutativeSemigroup semigroup) =
                        monoid.semigroup

                    inversePlusA =
                        semigroup.prepend
                            (group.inverse a)
                            a

                    aPlusInverse =
                        semigroup.prepend
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
                    (Typeclasses.Classes.AbelianGroup.AbelianGroup group) =
                        Typeclasses.Classes.AbelianGroup.exclusiveOr

                    (Typeclasses.Classes.CommutativeMonoid.CommutativeMonoid monoid) =
                        group.monoid

                    (Typeclasses.Classes.CommutativeSemigroup.CommutativeSemigroup semigroup) =
                        monoid.semigroup

                    inverseXorA =
                        semigroup.prepend
                            (group.inverse a)
                            a

                    aXorInverse =
                        semigroup.prepend
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
                    (Typeclasses.Classes.AbelianGroup.AbelianGroup group) =
                        Typeclasses.Classes.AbelianGroup.modularArithmetic 12

                    (Typeclasses.Classes.CommutativeMonoid.CommutativeMonoid monoid) =
                        group.monoid

                    (Typeclasses.Classes.CommutativeSemigroup.CommutativeSemigroup semigroup) =
                        monoid.semigroup

                    inversePlusA =
                        semigroup.prepend
                            (group.inverse a)
                            a

                    aPlusInverse =
                        semigroup.prepend
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
