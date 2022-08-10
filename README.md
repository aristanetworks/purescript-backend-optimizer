# purescript-backend-transmogrifier

An optimizing backend toolkit for PureScript's CoreFn.

## Overview

PureScript's built-in optimizer leaves a lot on the table by only performing
naive syntactic rewrites in the JavaScript specific backend. `purescript-backend-transmogrifier`
consumes the compiler's high-level IR (`CoreFn`) and applies a more aggressive
inlining pipeline (subsuming existing optimizations) that is backend agnostic.

It additionally ships with an alternative codegenerator which outputs modern ECMAScript
with additional runtime optimizations, resulting in lighter, faster bundles.
