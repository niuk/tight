# The Language

## Features

### Basics

We use the single colon (`:`) for both type and kind annotation. `*` is the kind of all types that are inhabited by values. `()` is the unit type and `(t1, ..., tn)` constructs tuples and tuple types with `n` elements.

### Linearity

Same as Morris, we use constraints to distinguish between linear and unrestricted types. Specifically, unrestrictedness is represented by this constraint:

```
type Unrestricted: * -> *
type Unrestricted t = {
    duplicate: t -> (t, t),
    drop: t -> (),
}
```

Types are treated as linear unless they satisfy this constraint.

A tuple type is linear if and only if all of its slots are occupied by linear types.

Additive tuples allow collections of linear computations to be suspended until one alternative is chosen:

```
let x = get_linear_resource()
let y = (foo(x) | bar(x))
let z = y.0
```

Here, `z` is the result of `foo(x)`. The expression `y.0` consumes the linear resource.

Lastly, conventional algebraic datatypes are also present to encode eager sum types:
```
type Maybe: * -> *
Nothing: forall a. Maybe a
Just: forall a. [] a -> Maybe a
```

### Functions

```
apply: forall a b c. [] ([a] b -> c) -> [[a] b -> c] b -> c
apply f = lambda x. f x
```

(Yes, it looks like C++.) The types closed by square brackets are the types of the objects captured by the closure, in no meaningful order. The size of a function is the sum of the sizes of these types plus the size of a code pointer. Function application is generic over collections of captured types. Similar to tuple types, a function type is considered linear if and only if all its captured types are linear.

This representation serves two purposes. First, it allows us to store functions simply as globs of plain old data, simplifying compilation and binary interfacing. Second, it allows us to derive the linearity of a function type directly from its parameters without resorting to subtype relations and gives us a single function type constructor instead of one for linear closures and another for unrestricted closures.

## Relevant Publications
* [Do Be Do Be Do. L Convent, S Lindley, C McBride, C McLaughlin](http://homepages.inf.ed.ac.uk/slindley/papers/frankly-draft-november2019.pdf)

* [Lightweight Linear Types in System F◦. K Mazurak, J Zhao, S Zdancewic](https://www.cis.upenn.edu/~stevez/papers/MZZ10.pdf)

* [The Best of Both Worlds: Linear Functional Programming without Compromise. J G Morris](https://arxiv.org/pdf/1612.06633.pdf)

* [On Regions and Linear Types. D Walker and K Watkins](http://www.cs.cmu.edu/~dpw/papers/lr-submitted.pdf)

* [Linear Regions Are All You Need. M Fluet, G Morrisett, A Ahmed](http://www.ccs.neu.edu/home/amal/papers/linrgn.pdf)

* [Adoption and Focus: Practical Linear Types for Imperative Programming. M Fähndrich, R DeLine](https://www.microsoft.com/en-us/research/wp-content/uploads/2002/05/pldi02.pdf?from=http%3A%2F%2Fresearch.microsoft.com%2F%7Emaf%2Fpapers%2Fpldi02.pdf)
