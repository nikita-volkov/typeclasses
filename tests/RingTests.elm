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

        -- , Test.fuzz
        --     Fuzz.unit
        --     "tests trivial group has an inverse"
        --   <|
        --     \a ->
        --         let
        --             inversePlusA =
        --                 Typeclasses.Classes.Group.trivialGroup.monoid.semigroup.prepend
        --                     (Typeclasses.Classes.Group.trivialGroup.inverse a)
        --                     a
        --             aPlusInverse =
        --                 Typeclasses.Classes.Group.trivialGroup.monoid.semigroup.prepend
        --                     a
        --                     (Typeclasses.Classes.Group.trivialGroup.inverse a)
        --         in
        --         Expect.true "All equal a"
        --             (inversePlusA
        --                 == Typeclasses.Classes.Group.trivialGroup.monoid.identity
        --                 && aPlusInverse
        --                 == Typeclasses.Classes.Group.trivialGroup.monoid.identity
        --             )
        -- , Test.fuzz
        --     Fuzz.bool
        --     "tests exclusiveOr has an inverse"
        --   <|
        --     \a ->
        --         let
        --             inverseXorA =
        --                 Typeclasses.Classes.Group.exclusiveOr.monoid.semigroup.prepend
        --                     (Typeclasses.Classes.Group.exclusiveOr.inverse a)
        --                     a
        --             aXorInverse =
        --                 Typeclasses.Classes.Group.exclusiveOr.monoid.semigroup.prepend
        --                     a
        --                     (Typeclasses.Classes.Group.exclusiveOr.inverse a)
        --         in
        --         Expect.true "All equal a"
        --             (inverseXorA
        --                 == Typeclasses.Classes.Group.exclusiveOr.monoid.identity
        --                 && aXorInverse
        --                 == Typeclasses.Classes.Group.exclusiveOr.monoid.identity
        --             )
        ]
