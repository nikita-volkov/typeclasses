module RingTests exposing (suite)

import Expect
import Fuzz
import Test
import Typeclasses.Classes.AbelianGroup
import Typeclasses.Classes.CommutativeMonoid
import Typeclasses.Classes.CommutativeRing
import Typeclasses.Classes.CommutativeSemigroup
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
                    (Typeclasses.Classes.CommutativeRing.CommutativeRing commutativeRing) =
                        Typeclasses.Classes.CommutativeRing.trivialRing

                    multiplication =
                        commutativeRing.multiplication

                    addition =
                        commutativeRing.addition

                    (Typeclasses.Classes.CommutativeMonoid.CommutativeMonoid multiplicationCommutativeMonoid) =
                        multiplication

                    (Typeclasses.Classes.CommutativeSemigroup.CommutativeSemigroup multiplicationCommutativeSemigroup) =
                        multiplicationCommutativeMonoid.semigroup

                    (Typeclasses.Classes.AbelianGroup.AbelianGroup additionAbelianGroup) =
                        addition

                    (Typeclasses.Classes.CommutativeMonoid.CommutativeMonoid additionCommutativeMonoid) =
                        additionAbelianGroup.monoid

                    (Typeclasses.Classes.CommutativeSemigroup.CommutativeSemigroup additionCommutativeSemigroup) =
                        additionCommutativeMonoid.semigroup

                    xTimesYPlusZ =
                        multiplicationCommutativeSemigroup
                            x
                            (additionCommutativeSemigroup y z)

                    xTimesY =
                        multiplicationCommutativeSemigroup x y

                    xTimesZ =
                        multiplicationCommutativeSemigroup x z

                    xTimesYPlusXTimesZ =
                        additionCommutativeSemigroup xTimesY xTimesZ

                    xPlusYTimesZ =
                        multiplicationCommutativeSemigroup
                            (additionCommutativeSemigroup x y)
                            z

                    yTimesZ =
                        multiplicationCommutativeSemigroup y z

                    xTimesZPlusYTimesZ =
                        additionCommutativeSemigroup xTimesZ yTimesZ
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
                    (Typeclasses.Classes.CommutativeRing.CommutativeRing commutativeRing) =
                        Typeclasses.Classes.CommutativeRing.exclusiveOrRing

                    multiplication =
                        commutativeRing.multiplication

                    addition =
                        commutativeRing.addition

                    (Typeclasses.Classes.CommutativeMonoid.CommutativeMonoid multiplicationCommutativeMonoid) =
                        multiplication

                    (Typeclasses.Classes.CommutativeSemigroup.CommutativeSemigroup multiplicationCommutativeSemigroup) =
                        multiplicationCommutativeMonoid.semigroup

                    (Typeclasses.Classes.AbelianGroup.AbelianGroup additionAbelianGroup) =
                        addition

                    (Typeclasses.Classes.CommutativeMonoid.CommutativeMonoid additionCommutativeMonoid) =
                        additionAbelianGroup.monoid

                    (Typeclasses.Classes.CommutativeSemigroup.CommutativeSemigroup additionCommutativeSemigroup) =
                        additionCommutativeMonoid.semigroup

                    xAllYXorZ =
                        multiplicationCommutativeSemigroup
                            x
                            (additionCommutativeSemigroup y z)

                    xAllY =
                        multiplicationCommutativeSemigroup x y

                    xAllZ =
                        multiplicationCommutativeSemigroup x z

                    xAllYXorXAllZ =
                        additionCommutativeSemigroup xAllY xAllZ

                    xXorYAllZ =
                        multiplicationCommutativeSemigroup
                            (additionCommutativeSemigroup x y)
                            z

                    yAllZ =
                        multiplicationCommutativeSemigroup y z

                    xAllZXorYAllZ =
                        additionCommutativeSemigroup xAllZ yAllZ
                in
                Expect.true "All equal a"
                    (xAllYXorZ
                        == xAllYXorXAllZ
                        && xXorYAllZ
                        == xAllZXorYAllZ
                    )
        ]
