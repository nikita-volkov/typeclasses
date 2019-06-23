# Summary

They say Elm doesn't support typeclasses.
What they actually mean is that it doesn't support implicit values.
Typeclasses can be defined as records with their instances provided as explicit values.
This package does exactly that.

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
