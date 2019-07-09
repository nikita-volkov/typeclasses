# Summary

They say Elm doesn't support typeclasses.
What they actually mean is that it doesn't support implicit values.
Typeclasses can be defined as records with their instances provided as explicit values.
Elm has problems, which typeclasses can solve.
This package aims to do exactly that.

# Motivation

Elm has a rough edge in its design when it comes to generic operations:
the compiler provides several functions which magically generalize over numeric, comparable and appendable types
(for details see [the "Constrained type variables" section of Elm Guide](https://guide.elm-lang.org/types/reading_types.html#constrained-type-variables)).

This comes with several consequences:

1. You only have a limited set of predefined generic APIs, which is just numeric, appending and comparison ops:
    - You cannot add support of such APIs to your custom types;
    - You cannot define new generic APIs, e.g.: a hashing function, a custom conversion to string, a binary codec, an arbitrary value generator for Fuzz-tests (aka Property-tests).
1. Because `Set` requires its elements to be comparable, it limits you to `Int`, `Float`, `Char`, `String`, and lists/tuples of such values. You cannot have custom types there. This goes beyond just `Set`.
1. Normally you're free to choose names for type parameters. These ones you have to name strictly `comparable` or `appendable` and etc.
1. To allow you to have a thing that is both `comparable` or `appendable` the compiler has to employ another level of workarounds: a combined constraint `compappend`.

# Solution

Let's take the comparable values for example.
We can define a type like the following, which will contain a dictionary of the same operations as the `comparable` magic constraint provides:

```elm
type alias Comparison a =
  {
    compare : a -> a -> Order,
    lt : a -> a -> Bool,
    le : a -> a -> Bool,
    gt : a -> a -> Bool,
    ge : a -> a -> Bool,
    min : a -> a -> a,
    max : a -> a -> a
  }
```

Then we can instantiate it for specific types, e.g.:

```elm
int : Comparison Int
int =
  {
    compare = compare,
    lt = (<),
    le = (<=),
    gt = (>),
    ge = (>=),
    min = min,
    max = max
  }
```

We can have helper functions, which would allow us to reduce the amount of operations to implement (I'll leave the details of implementation down):

```elm
fromCompare : (a -> a -> Order) -> Comparison a
```

Then we can define instances for our custom types:

```elm
type Quality = Low | High

comparison : Comparison Quality
comparison = fromCompare <| \ left right -> case left of
  Low -> case right of
    Low -> EQ
    _ -> LT
  High -> case right of
    High -> EQ
    _ -> GT
```

And we can design generic APIs without magical constraints.
Meaning that they'll work for any type that you can provide `Comparison` for:

```elm
module Set exposing (..)

fromList : Comparison a -> List a -> Set a
```

What you've seen here can be defined thus:
- `type alias Comparison a` is a declaration of a typeclass
- `int : Comparison Int` is an instance of a typeclass
- `fromList : Comparison a -> List a -> Set a` is a polymorphic function constrained by a typeclass

There! If you haven't before, now you know typeclasses. It's that simple.

# Status

This library is in a stage of active development and not yet well covered with tests,
which is why it is not yet advised to rely production systems on it.
To change the status sooner you're welcome to PR with tests, benchmarks and
missing instance declarations for basic types.

# FAQ

## Why no Functors, Monads and alike?

Unfortunately Elm's type system has two limitations:

* it doesn't support `forall` quantification;
* it doesn't support higher-kinded polymorphism.

Having such features we would be able to provide definitions such as the following:

```elm
type alias Functor f =
  {
    map : forall a b. (a -> b) -> f a -> f b
  }

list : Functor List
list = { map = List.map }
```

However until we get these features the best we can do is something like this:

```elm
type alias Functor a b fa fb =
  {
    map : (a -> b) -> fa -> fb
  }

list : Functor a b (List a) (List b)
list = { map = List.map }
```

Because such type signatures seem likely to breed terrible APIs, we haven't yet dived into studying this opportunity deeper and limited this library to classes which at least don't require higher-kinded polymorphism.

# Acknowledgements

The work behind this library was much inspired by the ideas expressed in [the "Scrap your typeclasses" Haskell blogpost](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html) by Gabriel Gonzalez.
