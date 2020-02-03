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
        ]
