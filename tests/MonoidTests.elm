module MonoidTests exposing (suite)

import Expect
import Fuzz
import Test
import Typeclasses.Classes.Monoid


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
        , Test.fuzz
            Fuzz.unit
            "tests () is commutattive"
          <|
            \a ->
                let
                    aOrIdentity =
                        Typeclasses.Classes.Monoid.unit.semigroup.prepend a Typeclasses.Classes.Monoid.unit.identity

                    identityOrA =
                        Typeclasses.Classes.Monoid.unit.semigroup.prepend Typeclasses.Classes.Monoid.unit.identity a
                in
                Expect.true "All equal a" (aOrIdentity == a && identityOrA == a)
        , Test.fuzz
            Fuzz.bool
            "tests exclusiveOr identity is identity and commutative"
          <|
            \a ->
                let
                    aXOrIdentity =
                        Typeclasses.Classes.Monoid.exclusiveOr.semigroup.prepend a Typeclasses.Classes.Monoid.exclusiveOr.identity

                    identityXOrA =
                        Typeclasses.Classes.Monoid.exclusiveOr.semigroup.prepend Typeclasses.Classes.Monoid.exclusiveOr.identity a
                in
                Expect.true "All equal a" (aXOrIdentity == a && identityXOrA == a)
        , Test.fuzz
            Fuzz.int
            "tests modularArithmetic identity is identity and commutative"
          <|
            \a ->
                let
                    aTimesIdentity =
                        (Typeclasses.Classes.Monoid.modularArithmetic 12).semigroup.prepend a (Typeclasses.Classes.Monoid.modularArithmetic 12).identity

                    identityTimesA =
                        (Typeclasses.Classes.Monoid.modularArithmetic 12).semigroup.prepend (Typeclasses.Classes.Monoid.modularArithmetic 12).identity a
                in
                Expect.true "All equal a" (aTimesIdentity == Basics.modBy 12 a && identityTimesA == Basics.modBy 12 a)
        ]
