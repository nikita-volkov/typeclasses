module MonoidTests exposing (suite)

import Expect
import Fuzz
import Monoid
import Test


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
                        Monoid.string.semigroup a Monoid.string.identity

                    identityAppendA =
                        Monoid.string.semigroup Monoid.string.identity a
                in
                Expect.true "All equal a" (aAppendIdentity == a && identityAppendA == a)
        , Test.fuzz
            (Fuzz.maybe Fuzz.unit)
            "tests maybeFirst identity is identity and commutative"
          <|
            \a ->
                let
                    aMaybeFirstIdentity =
                        Monoid.maybeFirst.semigroup a Monoid.maybeFirst.identity

                    identityMaybeFirstA =
                        Monoid.maybeFirst.semigroup Monoid.maybeFirst.identity a
                in
                Expect.true "All equal a" (aMaybeFirstIdentity == a && identityMaybeFirstA == a)
        , Test.fuzz
            (Fuzz.list Fuzz.unit)
            "tests list identity is identity and commutative"
          <|
            \a ->
                let
                    aAppendIdentity =
                        Monoid.list.semigroup a Monoid.list.identity

                    identityAppendA =
                        Monoid.list.semigroup Monoid.list.identity a
                in
                Expect.true "All equal a" (aAppendIdentity == a && identityAppendA == a)
        ]
