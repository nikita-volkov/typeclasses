module GroupTests exposing (suite)

import Expect
import Fuzz
import Test
import Typeclasses.Classes.Group


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
                        Typeclasses.Classes.Group.numberSum.monoid.semigroup.prepend
                            (Typeclasses.Classes.Group.numberSum.inverse a)
                            a

                    aPlusInverse =
                        Typeclasses.Classes.Group.numberSum.monoid.semigroup.prepend
                            a
                            (Typeclasses.Classes.Group.numberSum.inverse a)
                in
                Expect.true "All equal a"
                    (inversePlusA
                        == Typeclasses.Classes.Group.numberSum.monoid.identity
                        && aPlusInverse
                        == Typeclasses.Classes.Group.numberSum.monoid.identity
                    )
        , Test.fuzz
            Fuzz.unit
            "tests trivial group has an inverse"
          <|
            \a ->
                let
                    inversePlusA =
                        Typeclasses.Classes.Group.trivialGroup.monoid.semigroup.prepend
                            (Typeclasses.Classes.Group.trivialGroup.inverse a)
                            a

                    aPlusInverse =
                        Typeclasses.Classes.Group.trivialGroup.monoid.semigroup.prepend
                            a
                            (Typeclasses.Classes.Group.trivialGroup.inverse a)
                in
                Expect.true "All equal a"
                    (inversePlusA
                        == Typeclasses.Classes.Group.trivialGroup.monoid.identity
                        && aPlusInverse
                        == Typeclasses.Classes.Group.trivialGroup.monoid.identity
                    )
        , Test.fuzz
            Fuzz.bool
            "tests exclusiveOr has an inverse"
          <|
            \a ->
                let
                    inverseXorA =
                        Typeclasses.Classes.Group.exclusiveOr.monoid.semigroup.prepend
                            (Typeclasses.Classes.Group.exclusiveOr.inverse a)
                            a

                    aXorInverse =
                        Typeclasses.Classes.Group.exclusiveOr.monoid.semigroup.prepend
                            a
                            (Typeclasses.Classes.Group.exclusiveOr.inverse a)
                in
                Expect.true "All equal a"
                    (inverseXorA
                        == Typeclasses.Classes.Group.exclusiveOr.monoid.identity
                        && aXorInverse
                        == Typeclasses.Classes.Group.exclusiveOr.monoid.identity
                    )
        , Test.fuzz
            Fuzz.int
            "tests modularArithmetic has an inverse"
          <|
            \a ->
                let
                    inversePlusA =
                        (Typeclasses.Classes.Group.modularArithmetic 12).monoid.semigroup.prepend
                            ((Typeclasses.Classes.Group.modularArithmetic 12).inverse a)
                            a

                    aPlusInverse =
                        (Typeclasses.Classes.Group.modularArithmetic 12).monoid.semigroup.prepend
                            a
                            ((Typeclasses.Classes.Group.modularArithmetic 12).inverse a)
                in
                Expect.true "All equal a"
                    (inversePlusA
                        == (Typeclasses.Classes.Group.modularArithmetic 12).monoid.identity
                        && aPlusInverse
                        == (Typeclasses.Classes.Group.modularArithmetic 12).monoid.identity
                    )
        ]
