module RingTests exposing (suite)

import AbelianGroup
import CommutativeMonoid
import CommutativeRing
import CommutativeSemigroup
import Expect
import Fuzz
import Ring
import Test


suite : Test.Test
suite =
    Test.describe "The Ring abstraction"
        [ Test.fuzz3
            Fuzz.unit
            Fuzz.unit
            Fuzz.unit
            "tests trivialRing all distributes over unit"
          <|
            \x y z ->
                let
                    (AbelianGroup.AbelianGroup groupAddition) =
                        Ring.trivial.addition

                    xLeftMultiplyYplusZ =
                        Ring.trivial.multiplication.semigroup
                            x
                            (groupAddition.monoid.semigroup y z)

                    yLeftMultiplyX =
                        Ring.trivial.multiplication.semigroup x y

                    zLeftMultiplyX =
                        Ring.trivial.multiplication.semigroup x z

                    yLeftMultiplyXPlusZLeftMultiplyX =
                        groupAddition.monoid.semigroup yLeftMultiplyX zLeftMultiplyX

                    xPlusYRightMultiplyZ =
                        Ring.trivial.multiplication.semigroup
                            (groupAddition.monoid.semigroup x y)
                            z

                    xRightMultiplyZ =
                        Ring.trivial.multiplication.semigroup x z

                    yRightMultiplyZ =
                        Ring.trivial.multiplication.semigroup y z

                    xRightMultiplyZPlusYRightMultiplyZ =
                        groupAddition.monoid.semigroup xRightMultiplyZ yRightMultiplyZ
                in
                Expect.true "All equal a"
                    (xLeftMultiplyYplusZ
                        == yLeftMultiplyXPlusZLeftMultiplyX
                    )
        , Test.fuzz3
            Fuzz.bool
            Fuzz.bool
            Fuzz.bool
            "tests exclusiveOrRing all distributes over xor"
          <|
            \x y z ->
                let
                    (AbelianGroup.AbelianGroup groupAddition) =
                        Ring.exclusiveOr.addition

                    xLeftMultiplyYplusZ =
                        Ring.exclusiveOr.multiplication.semigroup
                            x
                            (groupAddition.monoid.semigroup y z)

                    yLeftMultiplyX =
                        Ring.exclusiveOr.multiplication.semigroup x y

                    zLeftMultiplyX =
                        Ring.exclusiveOr.multiplication.semigroup x z

                    yLeftMultiplyXPlusZLeftMultiplyX =
                        groupAddition.monoid.semigroup yLeftMultiplyX zLeftMultiplyX

                    xPlusYRightMultiplyZ =
                        Ring.exclusiveOr.multiplication.semigroup
                            (groupAddition.monoid.semigroup x y)
                            z

                    xRightMultiplyZ =
                        Ring.exclusiveOr.multiplication.semigroup x z

                    yRightMultiplyZ =
                        Ring.exclusiveOr.multiplication.semigroup y z

                    xRightMultiplyZPlusYRightMultiplyZ =
                        groupAddition.monoid.semigroup xRightMultiplyZ yRightMultiplyZ
                in
                Expect.true "All equal a"
                    (xLeftMultiplyYplusZ
                        == yLeftMultiplyXPlusZLeftMultiplyX
                    )
        , Test.fuzz3
            (Fuzz.intRange 1 10)
            (Fuzz.intRange 1 10)
            (Fuzz.intRange 1 10)
            "tests numberRing all distributes over numberSum"
          <|
            \x y z ->
                let
                    (AbelianGroup.AbelianGroup groupAddition) =
                        Ring.number.addition

                    xLeftMultiplyYplusZ =
                        Ring.number.multiplication.semigroup
                            x
                            (groupAddition.monoid.semigroup y z)

                    yLeftMultiplyX =
                        Ring.number.multiplication.semigroup x y

                    zLeftMultiplyX =
                        Ring.number.multiplication.semigroup x z

                    yLeftMultiplyXPlusZLeftMultiplyX =
                        groupAddition.monoid.semigroup yLeftMultiplyX zLeftMultiplyX

                    xPlusYRightMultiplyZ =
                        Ring.number.multiplication.semigroup
                            (groupAddition.monoid.semigroup x y)
                            z

                    xRightMultiplyZ =
                        Ring.number.multiplication.semigroup x z

                    yRightMultiplyZ =
                        Ring.number.multiplication.semigroup y z

                    xRightMultiplyZPlusYRightMultiplyZ =
                        groupAddition.monoid.semigroup xRightMultiplyZ yRightMultiplyZ
                in
                Expect.true "All equal a"
                    (xLeftMultiplyYplusZ
                        == yLeftMultiplyXPlusZLeftMultiplyX
                    )
        ]
