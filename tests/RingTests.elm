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
    Test.describe "The Group abstraction"
        [ Test.fuzz3
            Fuzz.unit
            Fuzz.unit
            Fuzz.unit
            "tests trivialRing multiplication distributes over addition"
          <|
            \x y z ->
                let
                    (AbelianGroup.AbelianGroup groupAddition) =
                        Ring.trivialRing.addition

                    xTimesYPlusZ =
                        Ring.trivialRing.multiplication.semigroup
                            x
                            (groupAddition.monoid.semigroup y z)

                    xTimesY =
                        Ring.trivialRing.multiplication.semigroup x y

                    xTimesZ =
                        Ring.trivialRing.multiplication.semigroup x z

                    xTimesYPlusXTimesZ =
                        groupAddition.monoid.semigroup xTimesY xTimesZ

                    xPlusYTimesZ =
                        Ring.trivialRing.multiplication.semigroup
                            (groupAddition.monoid.semigroup x y)
                            z

                    yTimesZ =
                        Ring.trivialRing.multiplication.semigroup y z

                    xTimesZPlusYTimesZ =
                        groupAddition.monoid.semigroup xTimesZ yTimesZ
                in
                Expect.true "All equal a"
                    (xTimesYPlusZ
                        == xTimesYPlusXTimesZ
                        && xPlusYTimesZ
                        == xTimesZPlusYTimesZ
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

                    xAllYXorZ =
                        Ring.exclusiveOrRing.multiplication.semigroup
                            x
                            (groupAddition.monoid.semigroup y z)

                    xAllY =
                        Ring.exclusiveOrRing.multiplication.semigroup x y

                    xAllZ =
                        Ring.exclusiveOrRing.multiplication.semigroup x z

                    xAllYXorXAllZ =
                        groupAddition.monoid.semigroup xAllY xAllZ

                    xXorYAllZ =
                        Ring.exclusiveOrRing.multiplication.semigroup
                            (groupAddition.monoid.semigroup x y)
                            z

                    yAllZ =
                        Ring.exclusiveOrRing.multiplication.semigroup y z

                    xAllZXorYAllZ =
                        groupAddition.monoid.semigroup xAllZ yAllZ
                in
                Expect.true "All equal a"
                    (xAllYXorZ
                        == xAllYXorXAllZ
                        && xXorYAllZ
                        == xAllZXorYAllZ
                    )
        ]
