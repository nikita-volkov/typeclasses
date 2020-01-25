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
            Fuzz.int
            "tests numberProduct identity is identity and commutative"
          <|
            \a ->
                let
                    aTimesIdentity =
                        Typeclasses.Classes.Monoid.intProduct.semigroup.prepend a Typeclasses.Classes.Monoid.intProduct.identity

                    identityTimesA =
                        Typeclasses.Classes.Monoid.intProduct.semigroup.prepend Typeclasses.Classes.Monoid.intProduct.identity a
                in
                Expect.true "All equal a" (aTimesIdentity == a && identityTimesA == a)
        , Test.fuzz
            Fuzz.int
            "tests numberSum identity is identity and commutative"
          <|
            \a ->
                let
                    aPlusIdentity =
                        Typeclasses.Classes.Monoid.numberSum.semigroup.prepend a Typeclasses.Classes.Monoid.numberSum.identity

                    identityPlusA =
                        Typeclasses.Classes.Monoid.numberSum.semigroup.prepend Typeclasses.Classes.Monoid.numberSum.identity a
                in
                Expect.true "All equal a" (aPlusIdentity == a && identityPlusA == a)
        , Test.fuzz
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
        , Test.fuzz
            Fuzz.bool
            "tests all identity is identity and commutative"
          <|
            \a ->
                let
                    aAndIdentity =
                        Typeclasses.Classes.Monoid.all.semigroup.prepend a Typeclasses.Classes.Monoid.all.identity

                    identityAndA =
                        Typeclasses.Classes.Monoid.all.semigroup.prepend Typeclasses.Classes.Monoid.all.identity a
                in
                Expect.true "All equal a" (aAndIdentity == a && identityAndA == a)
        , Test.fuzz
            Fuzz.bool
            "tests any identity is identity and commutative"
          <|
            \a ->
                let
                    aOrIdentity =
                        Typeclasses.Classes.Monoid.any.semigroup.prepend a Typeclasses.Classes.Monoid.any.identity

                    identityOrA =
                        Typeclasses.Classes.Monoid.any.semigroup.prepend Typeclasses.Classes.Monoid.any.identity a
                in
                Expect.true "All equal a" (aOrIdentity == a && identityOrA == a)
        ]
