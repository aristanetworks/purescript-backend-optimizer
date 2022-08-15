# purescript-backend-optimizer

An optimizing backend toolkit for PureScript's CoreFn.

## Overview

PureScript's built-in optimizer leaves a lot on the table by only performing
naive syntactic rewrites in the JavaScript specific backend.
`purescript-backend-optimizer` consumes the compiler's high-level IR (`CoreFn`)
and applies a more aggressive inlining pipeline (subsuming existing
optimizations) that is backend agnostic.

It additionally ships with an alternative code-generator which outputs modern
ECMAScript with additional runtime optimizations, resulting in lighter, faster
bundles.

| Example | Input | `purs-backend-es` |
|---------|-------|-------------------|
| Lenses | [Input](./backend-es/test/snapshots/Snapshot.ProfunctorLenses01.purs) | [Output](./backend-es/test/snapshots-out/Snapshot.ProfunctorLenses01.js) |
| Prisms | [Input](./backend-es/test/snapshots/Snapshot.ProfunctorLenses02.purs) | [Output](./backend-es/test/snapshots-out/Snapshot.ProfunctorLenses02.js) |
| Variant | [Input](./backend-es/test/snapshots/Snapshot.Variant01.purs) | [Output](./backend-es/test/snapshots-out/Snapshot.Variant01.js) |
| Heterogeneous | [Input](./backend-es/test/snapshots/Snapshot.Heterogeneous01.purs) | [Output](./backend-es/test/snapshots-out/Snapshot.Heterogeneous01.js) |
| Uncurried CPS | [Input](./backend-es/test/snapshots/Snapshot.Cps02.purs) | [Output](./backend-es/test/snapshots-out/Snapshot.Cps02.js) |
| Generics | [Input](./backend-es/test/snapshots/Snapshot.KnownConstructors06.purs) | [Output](./backend-es/test/snapshots-out/Snapshot.KnownConstructors06.js) |
| Fusion (Fold) | [Input](./backend-es/test/snapshots/Snapshot.Fusion01.purs) | [Output](./backend-es/test/snapshots-out/Snapshot.Fusion01.js) |
| Fusion (Unfold) | [Input](./backend-es/test/snapshots/Snapshot.Fusion02.purs) | [Output](./backend-es/test/snapshots-out/Snapshot.Fusion02.js) |
| Recursion Schemes | [Input](./backend-es/test/snapshots/Snapshot.RecursionSchemes01.purs) | [Output](./backend-es/test/snapshots-out/Snapshot.RecursionSchemes01.js) |

## ECMAScript Backend

### Install

```sh
npm install purs-backend-es
```

### Usage

`purs-backend-es` can be added as a backend in your `spago.dhall`.

```diff
+, backend = "purs-backend-es"
```

_You should likely only do this for a production build configuration_, since
optimization and code-generation are currently not incremental.

By default, `purs-backend-es` will read corefn.json files from `output`, and
generate code in `output-es` following the same directory pattern as the
compiler's JS backend.

See the CLI help for options:

```sh
purs-backend-es --help
```

## Inlining Directives

The inliner follows some basic heuristics, but to get the most out of it you
should configure inlining directives. An inlining directive tells the optimizer
under what conditions it should inline a definition.

The following inlining directives are supported:

  * `default` - A definition is inlined using default heuristics (unspecified).
  * `never` - A definition is never inlined.
  * `always` - A definition is inlined at every reference.
  * `arity=n` - Where `n` is a positive integer, a definition is inlined when
    at least `n` arguments are applied.

An inlining directive may be applied to a top-level binding or top-level accessor.

### Syntax

```purescript
module Example where

import Prelude

myAdd :: Int -> Int -> Int
myAdd a b = a + b
```

The `myAdd` function would likely already be inlined since it is so small, but
to guarantee that it is always inlined after two argments are applied, you would
write the following directive:

```
Example.myAdd arity=2
```

For instance methods, you should use named instances and a top-level accessor:

```purescript
module Example where

import Prelude

data MyData = Zero | One

instance semigroupMyData :: Semigroup MyData where
  append = case _, _ of
    Zero, _ -> Zero
    _, Zero -> Zero
    _, _ -> One
```

```
Example.semigroupMyData.append arity=2
```

It's possible to refer to unnamed instances through their compiler-generated
name, however this is quite difficult to maintain.

### Configuration

Inlining directives can be configured in three ways:

#### Module-specific inlining directives via a module header

In any given module header you can add `@inline` comments with the above syntax:

```purescript
-- @inline Example.myAdd arity=2
module AnotherExample where

import Example
...
```

Directives configured this way only apply to the current module.

#### Global inlining directives via a module header

In any given module header, you can add `@inline export` directives for definitions
in the current module:

```purescript
-- @inline export myAdd arity=2
-- @inline export semigroupMyData.append arity=1
module Example where
...
```

Directives configured this way apply to the current module and downstream
modules.

*Note:* They must be defined in the module header to due to an upstream compiler
limitation.

#### Global inlining directives via a configuration file

You can provide a directive file to `purs-backend-es`:

```sh
purs-backend-es --directives my-directives.txt
```

Each line should contain an inlining directive using the above syntax, with the
additional support of `--` line comments. These directives will take precedence
over any defaults or exported directives, so you can tweak inlining for your
project however you see fit.

#### Cheatsheet

Precedence applies in the following order (most specific to least specific):

| Location | Affects |
|----------|---------|
| Module A's header, `@inline` module B directive | Module B's usages in module A |
| Directives file | All modules |
| Module A's header, `@inline export` module A directive | Module A's usages in all modules |
| Default heuristics | All modules |

## Semantics

`purescript-backend-optimizer` consumes the PureScript compiler's high-level
intermediate representation (IR) known as CoreFn. CoreFn has no defined
evaluation semantics, but we operate under assumptions based on common use:

* We makes decisions on what to keep or discard using
  [Fast and Loose
  Reasoning](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/fast+loose.pdf),
  assuming that CoreFn is _pure_ and _total_.

  In practical terms, this means we _may_ take the opportunity to remove any
  code that we know for certain is not demanded. However, at times we may also
  choose to propagate known bottoms. Thus, non-totality is considered _undefined
  behavior_ for the purposes of CoreFn's semantics.

* We preserve _sharing_ of redexes under common assumptions of strict evaluation.
  Like non-totality, we consider a specific evaluation order to be _undefined
  behavior_ in CoreFn. However, we assume that all terms under a redex should be
  in normal form.

  In practical terms, this means we will not delay function arguments that most
  would expect to be evaluated immediately.
