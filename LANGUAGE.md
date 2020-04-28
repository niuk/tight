# The Language

## Features

### Basics

We use the double colon (`::`) for both type and kind annotation. `*` is the kind of all types that are inhabited by values.

### Linearity

Same as Morris, we use constraints to distinguish between linear and unrestricted types:

```Haskell
type Un: * -> *
type Un t = {
    duplicate: t -> (t, t),
    drop: t -> (),
}
```

Types are treated as linear unless they satisfy this constraint.

Tuples are constructed by expressions separated with commas in parentheses. A tuple type is linear if and only if all of its slots are occupied by linear types.

```Haskell
tuple = (1, 2, 3)
(x, y, z) = tuple
r = x + y - z
```

Additive tuples allow collections of linear computations to be suspended until one alternative is chosen:

```Haskell
x = get_linear_resource()
y = (foo x | bar x)
z = y.0
```

Here, `z` is the result of `foo(x)`. The expression `y.0` consumes the linear resource.

Lastly, conventional algebraic datatypes are also present to encode eager sum types:

```Haskell
type Maybe: * -> *
Nothing: forall a. Maybe a
Just: forall a. a -> Maybe a
```

### Functions

We use arrows with tildes to represent linear functions. (There were no other ASCII symbols that looked good enough.)

```Haskell
type (->): * -> * -> *
type (~>): * -> * -> *

apply: forall f a b. Fun f => (a `f` b) -> a `f` b
apply f = lambda x. f x
```

The syntax `` `f` `` applies an expression infix.

### Interfaces

Interfaces are defined using record types:

```Haskell
type Eq: * -> *
type Eq a = {
    (==): forall b. [b] a -> a -> Bool
}
```

To use the functions of an interface implicitly, an implementation of the interface must exist in scope, and the interface must be mentioned in a type signature:

```Haskell
findIndex: forall a. (Eq a, Fun f, f <= a) => a -> List a `f` (Maybe Int, List a)
findIndex x = findStartingAt 0
  where
    findStartingAt: Int -> List a `f` (Maybe Int, List a)
    findStartingAt _ Empty = (Nothing, Empty)
    findStartingAt i (List y ys) =
        if x == y
        then (Just i, List y ys)
        else findStartingAt (i + 1) ys

use {
    (==) = intEq
}: Eq Int

index = findIndex 4 (0 `List` 3 `List` 4 `List` 2)
```

`List a` is a linear type, since a list is made up of dynamically allocated nodes that must be deallocated at some point. To preserve the list for subsequent usage, this function reconstructs it node by node at each step in the traversal. The compiler should systematically optimize away such patterns of destruction and reconstruction into simple reads.

## Compilation



## Relevant Links

* [Doo Bee Doo Bee Doo. L Convent, S Lindley, C McBride, C McLaughlin](http://lukas.convnet.de/proj/doo-bee/frankly-draft-february2019.pdf)

* [Lightweight Linear Types in System F◦. K Mazurak, J Zhao, S Zdancewic](https://www.cis.upenn.edu/~stevez/papers/MZZ10.pdf)

* [The Best of Both Worlds: Linear Functional Programming without Compromise. J G Morris](https://arxiv.org/pdf/1612.06633.pdf)

* [On Regions and Linear Types. D Walker and K Watkins](http://www.cs.cmu.edu/~dpw/papers/lr-submitted.pdf)

* [Adoption and Focus: Practical Linear Types for Imperative Programming. M Fähndrich, R DeLine](https://www.microsoft.com/en-us/research/wp-content/uploads/2002/05/pldi02.pdf?from=http%3A%2F%2Fresearch.microsoft.com%2F%7Emaf%2Fpapers%2Fpldi02.pdf)
