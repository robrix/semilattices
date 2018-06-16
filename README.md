# `semilattices`

This Haskell package defines typeclasses for join- and meet-semilattices, and for upper and lower bounds, and a variety of instances for each.


## Usage

Semilattices are idempotent commutative semigroups, and come in two flavours: `Join` and `Meet`. This presentation of them doesn’t inherit from `Semigroup` however, since `Semigroup`s already exist and the relationships between the various classes here warrant their own operators.

Join semilattices can be combined using the `\/` operator (pronounced “lub,” for “least upper bound”). Meet semilattices can be combined with the `/\` operator (pronounced “glb,” for “greatest lower bound”). They have opposite relationships to `Lower` and `Upper` bounds (which are optional; in general, there are more lower bounds than upper ones).


## Related work

- [`lattices`](http://hackage.haskell.org/package/lattices) also offers join & meet semilattices, bounds, & a variety of operations for instances. Relative to `lattices`, `semilattices` primarily offers a different class hierarchy, e.g. `Lower` & `Upper` do not have `Join` & `Meet` as superclasses.
