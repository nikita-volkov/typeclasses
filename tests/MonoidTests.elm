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
        , Test.fuzz
            Fuzz.int
            "tests numberSum identity is identity and commutative"
          <|
            \a ->
                let
                    aAppendIdentity =
                        Monoid.numberSum.semigroup a Monoid.numberSum.identity

                    identityAppendA =
                        Monoid.numberSum.semigroup Monoid.numberSum.identity a
                in
                Expect.true "All equal a" (aAppendIdentity == a && identityAppendA == a)
        , Test.fuzz
            Fuzz.int
            "tests numberProduct identity is identity and commutative"
          <|
            \a ->
                let
                    aAppendIdentity =
                        Monoid.numberProduct.semigroup a Monoid.numberProduct.identity

                    identityAppendA =
                        Monoid.numberProduct.semigroup Monoid.numberProduct.identity a
                in
                Expect.true "All equal a" (aAppendIdentity == a && identityAppendA == a)
        , Test.fuzz
            Fuzz.bool
            "tests all identity is identity and commutative"
          <|
            \a ->
                let
                    aAppendIdentity =
                        Monoid.all.semigroup a Monoid.all.identity

                    identityAppendA =
                        Monoid.all.semigroup Monoid.all.identity a
                in
                Expect.true "All equal a" (aAppendIdentity == a && identityAppendA == a)
        , Test.fuzz
            Fuzz.bool
            "tests any identity is identity and commutative"
          <|
            \a ->
                let
                    aAppendIdentity =
                        Monoid.any.semigroup a Monoid.any.identity

                    identityAppendA =
                        Monoid.any.semigroup Monoid.any.identity a
                in
                Expect.true "All equal a" (aAppendIdentity == a && identityAppendA == a)
        , Test.fuzz
            Fuzz.bool
            "tests exclusiveOr identity is identity and commutative"
          <|
            \a ->
                let
                    aAppendIdentity =
                        Monoid.exclusiveOr.semigroup a Monoid.exclusiveOr.identity

                    identityAppendA =
                        Monoid.exclusiveOr.semigroup Monoid.exclusiveOr.identity a
                in
                Expect.true "All equal a" (aAppendIdentity == a && identityAppendA == a)
        , Test.fuzz
            (Fuzz.intRange 0 11)
            "tests modularArithmetic identity is identity and commutative"
          <|
            \a ->
                let
                    aAppendIdentity =
                        (Monoid.modularArithmetic 12).semigroup a (Monoid.modularArithmetic 12).identity

                    identityAppendA =
                        (Monoid.modularArithmetic 12).semigroup (Monoid.modularArithmetic 12).identity a
                in
                Expect.true "All equal a" (aAppendIdentity == a && identityAppendA == a)
        ]
