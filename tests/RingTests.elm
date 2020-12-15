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
                        Ring.trivialRing.addition

                    xLeftMultiplyYplusZ =
                        Ring.trivialRing.multiplication.semigroup
                            x
                            (groupAddition.monoid.semigroup y z)

                    yLeftMultiplyX =
                        Ring.trivialRing.multiplication.semigroup x y

                    zLeftMultiplyX =
                        Ring.trivialRing.multiplication.semigroup x z

                    yLeftMultiplyXPlusZLeftMultiplyX =
                        groupAddition.monoid.semigroup yLeftMultiplyX zLeftMultiplyX

                    xPlusYRightMultiplyZ =
                        Ring.trivialRing.multiplication.semigroup
                            (groupAddition.monoid.semigroup x y)
                            z

                    xRightMultiplyZ =
                        Ring.trivialRing.multiplication.semigroup x z

                    yRightMultiplyZ =
                        Ring.trivialRing.multiplication.semigroup y z

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
                        Ring.exclusiveOrRing.addition

                    xLeftMultiplyYplusZ =
                        Ring.exclusiveOrRing.multiplication.semigroup
                            x
                            (groupAddition.monoid.semigroup y z)

                    yLeftMultiplyX =
                        Ring.exclusiveOrRing.multiplication.semigroup x y

                    zLeftMultiplyX =
                        Ring.exclusiveOrRing.multiplication.semigroup x z

                    yLeftMultiplyXPlusZLeftMultiplyX =
                        groupAddition.monoid.semigroup yLeftMultiplyX zLeftMultiplyX

                    xPlusYRightMultiplyZ =
                        Ring.exclusiveOrRing.multiplication.semigroup
                            (groupAddition.monoid.semigroup x y)
                            z

                    xRightMultiplyZ =
                        Ring.exclusiveOrRing.multiplication.semigroup x z

                    yRightMultiplyZ =
                        Ring.exclusiveOrRing.multiplication.semigroup y z

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
                        Ring.numberRing.addition

                    xLeftMultiplyYplusZ =
                        Ring.numberRing.multiplication.semigroup
                            x
                            (groupAddition.monoid.semigroup y z)

                    yLeftMultiplyX =
                        Ring.numberRing.multiplication.semigroup x y

                    zLeftMultiplyX =
                        Ring.numberRing.multiplication.semigroup x z

                    yLeftMultiplyXPlusZLeftMultiplyX =
                        groupAddition.monoid.semigroup yLeftMultiplyX zLeftMultiplyX

                    xPlusYRightMultiplyZ =
                        Ring.numberRing.multiplication.semigroup
                            (groupAddition.monoid.semigroup x y)
                            z

                    xRightMultiplyZ =
                        Ring.numberRing.multiplication.semigroup x z

                    yRightMultiplyZ =
                        Ring.numberRing.multiplication.semigroup y z

                    xRightMultiplyZPlusYRightMultiplyZ =
                        groupAddition.monoid.semigroup xRightMultiplyZ yRightMultiplyZ
                in
                Expect.true "All equal a"
                    (xLeftMultiplyYplusZ
                        == yLeftMultiplyXPlusZLeftMultiplyX
                    )
        ]
