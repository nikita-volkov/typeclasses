module MonoidTests exposing (suite)

import Expect
import Fuzz
import Test
import Typeclasses.Classes.Monoid
import Typeclasses.Classes.Semigroup


suite : Test.Test
suite =
    Test.describe "The Monoid abstraction"
        [ Test.fuzz
            Fuzz.string
            "tests string identity is identity and commutative"
          <|
            \a ->
                let
                    aAppendIdentity =
                        Typeclasses.Classes.Monoid.string.semigroup.prepend a Typeclasses.Classes.Monoid.string.identity

                    identityAppendA =
                        Typeclasses.Classes.Monoid.string.semigroup.prepend Typeclasses.Classes.Monoid.string.identity a
                in
                Expect.true "All equal a" (aAppendIdentity == a && identityAppendA == a)
        , Test.fuzz
            (Fuzz.maybe Fuzz.unit)
            "tests maybeFirst identity is identity and commutative"
          <|
            \a ->
                let
                    aMaybeFirstIdentity =
                        Typeclasses.Classes.Monoid.maybeFirst.semigroup.prepend a Typeclasses.Classes.Monoid.maybeFirst.identity

                    identityMaybeFirstA =
                        Typeclasses.Classes.Monoid.maybeFirst.semigroup.prepend Typeclasses.Classes.Monoid.maybeFirst.identity a
                in
                Expect.true "All equal a" (aMaybeFirstIdentity == a && identityMaybeFirstA == a)
        , Test.fuzz
            (Fuzz.list Fuzz.unit)
            "tests list identity is identity and commutative"
          <|
            \a ->
                let
                    aAppendIdentity =
                        Typeclasses.Classes.Monoid.list.semigroup.prepend a Typeclasses.Classes.Monoid.list.identity

                    identityAppendA =
                        Typeclasses.Classes.Monoid.list.semigroup.prepend Typeclasses.Classes.Monoid.list.identity a
                in
                Expect.true "All equal a" (aAppendIdentity == a && identityAppendA == a)
        ]
