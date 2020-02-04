module RingTests exposing (suite)

import Expect
import Fuzz
import Test
import Typeclasses.Classes.Ring


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
                    xTimesYPlusZ =
                        Typeclasses.Classes.Ring.trivialRing.multiplication.semigroup.prepend
                            x
                            (Typeclasses.Classes.Ring.trivialRing.addition.monoid.semigroup.prepend y z)

                    xTimesY =
                        Typeclasses.Classes.Ring.trivialRing.multiplication.semigroup.prepend x y

                    xTimesZ =
                        Typeclasses.Classes.Ring.trivialRing.multiplication.semigroup.prepend x z

                    xTimesYPlusXTimesZ =
                        Typeclasses.Classes.Ring.trivialRing.addition.monoid.semigroup.prepend xTimesY xTimesZ

                    xPlusYTimesZ =
                        Typeclasses.Classes.Ring.trivialRing.multiplication.semigroup.prepend
                            (Typeclasses.Classes.Ring.trivialRing.addition.monoid.semigroup.prepend x y)
                            z

                    yTimesZ =
                        Typeclasses.Classes.Ring.trivialRing.multiplication.semigroup.prepend y z

                    xTimesZPlusYTimesZ =
                        Typeclasses.Classes.Ring.trivialRing.addition.monoid.semigroup.prepend xTimesZ yTimesZ
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
                    xAllYXorZ =
                        Typeclasses.Classes.Ring.exclusiveOrRing.multiplication.semigroup.prepend
                            x
                            (Typeclasses.Classes.Ring.exclusiveOrRing.addition.monoid.semigroup.prepend y z)

                    xAllY =
                        Typeclasses.Classes.Ring.exclusiveOrRing.multiplication.semigroup.prepend x y

                    xAllZ =
                        Typeclasses.Classes.Ring.exclusiveOrRing.multiplication.semigroup.prepend x z

                    xAllYXorXAllZ =
                        Typeclasses.Classes.Ring.exclusiveOrRing.addition.monoid.semigroup.prepend xAllY xAllZ

                    xXorYAllZ =
                        Typeclasses.Classes.Ring.exclusiveOrRing.multiplication.semigroup.prepend
                            (Typeclasses.Classes.Ring.exclusiveOrRing.addition.monoid.semigroup.prepend x y)
                            z

                    yAllZ =
                        Typeclasses.Classes.Ring.exclusiveOrRing.multiplication.semigroup.prepend y z

                    xAllZXorYAllZ =
                        Typeclasses.Classes.Ring.exclusiveOrRing.addition.monoid.semigroup.prepend xAllZ yAllZ
                in
                Expect.true "All equal a"
                    (xAllYXorZ
                        == xAllYXorXAllZ
                        && xXorYAllZ
                        == xAllZXorYAllZ
                    )
        ]
